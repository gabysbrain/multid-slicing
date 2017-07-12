module App.Data where

import Prelude 
import App.Data.ServerData (SDEdges, SDEdge(..), SDPoints, SDPoint, ServerData(..))
import Control.Monad.Except (Except, except, runExcept, mapExcept, throwError)
import Data.DataFrame (DataFrame(..))
import Data.DataFrame as DF
import Data.Either (Either(..), either)
import Data.Foldable (class Foldable, foldMap, foldr)
import Data.Traversable (for, traverse)
import Data.Array as A
import Data.Array ((..), (!!))
import Data.List (List)
import Data.List as L
import Data.List.Types (NonEmptyList)
import Data.Newtype (class Newtype, unwrap)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Monoid (class Monoid, mempty)
import Data.Number (fromString)
import Data.String (Pattern(..), split, trim)
import Data.StrMap (StrMap)
import Data.StrMap as SM
import Data.Tuple (Tuple(..), fst, snd)
import Data.Formatter.Number (format)
import Data.Geom.Point (Point)
import Data.Geom.Point as P

newtype DataRow a = DataRow {rowId :: Int, row :: a}
type DataPoint d = DataRow (Point d)
type FieldNames d = Array String
type RawPoints d = DataFrame (DataPoint d)
type ParetoPoints d = DataFrame (DataPoint d)

derive instance newtypeDataRow :: Newtype (DataRow a) _

instance functorDataRow :: Functor DataRow where
  map f (DataRow r) = DataRow $ r {row = f r.row}

instance applyDataRow :: Apply DataRow where
  apply (DataRow f) (DataRow r) = DataRow $ f {row = f.row r.row}

rowVal :: forall a. DataRow a -> a
rowVal (DataRow r) = r.row

rowId :: forall a. DataRow a -> Int
rowId (DataRow r) = r.rowId

-- Used for low-level visualization
type PointData2D = {rowId :: Int, x :: Number, y :: Number, selected :: Boolean}
type LineData2D = 
  { slabId :: Int
  , selected :: Boolean
  , cosTheta :: Number
  , points :: Array PointData2D
  }

-- Used for the neighborhood graph
type Node d = DataPoint d
type Link d = {linkId :: Int, src :: Node d, tgt :: Node d}
type AngleLink d = {linkId :: Int, cosTheta :: Number, src :: Node d, tgt :: Node d}
type NeighborGraph d = {nodes :: DataFrame (Node d), links :: DataFrame (Link d)}

-- Universal number formatter
formatNum :: Number -> String
formatNum = format {comma: false, before: 0, after: 3, abbreviations: false, sign: false}

mergeErrs :: forall f n e. Semigroup e => Monoid n => Foldable f => 
             f (Except e n) -> Except e n
mergeErrs = foldr (\e1 e2 -> except $ merge' (runExcept e1) (runExcept e2))
                  (pure mempty)

merge' :: forall m n. Semigroup m => Semigroup n => Either m n -> Either m n -> Either m n
merge' (Left e1) (Left e2) = Left $ e1 <> e2
merge' (Right _) (Left e2) = Left e2
merge' (Left e1) (Right _) = Left e1
merge' (Right v1) (Right v2) = Right $ v1 <> v2

ptsFromServerData :: forall d
                   . SDPoints 
                  -> Except String (Tuple (FieldNames d) (ParetoPoints d))
ptsFromServerData pts = do
  r1 <- withFail "No data" $ A.head pts
  let fields = A.sort $ SM.keys r1
      ids = 1..(A.length pts)
  pts' :: Array (Point d) <- for pts $ \pt -> do
    ptArray :: Array Number <- traverse (lookupField pt) fields
    withFail "Invalid point size" $ P.fromArray ptArray
  let rows = A.zipWith (\r i -> DataRow {rowId:i, row: r}) pts' ids
  pure $ Tuple fields (DF.init rows)

ngFromServerData :: forall d. ParetoPoints d -> SDEdges -> Except String (NeighborGraph d)
ngFromServerData pts edges = do
  let ids = 1..(A.length edges)
  links <- for (A.zip ids edges) $ \(Tuple id (SDEdge edge)) -> do
    p1 <- lookupPoint pts edge.p1
    p2 <- lookupPoint pts edge.p2
    pure { linkId: id, src: p1, tgt: p2 }
  pure { nodes: pts, links: DF.init links }

lookupField :: StrMap Number -> String -> Except String Number
lookupField m f = withFail ("field " <> f <> " missing") $ SM.lookup f m

lookupPoint :: forall d. ParetoPoints d -> Int -> Except String (DataPoint d)
lookupPoint (DataFrame pts) i = 
  withFail ("row " <> (show i) <> " not found") $ pts !! (i-1)

withFail :: forall a. String -> Maybe a -> Except String a
withFail msg = maybe (throwError msg) pure

