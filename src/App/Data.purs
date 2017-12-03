module App.Data where

import Prelude 
import App.Data.ServerData ( SDCurves, SDCurve(..) 
                           , SDPoints, SDPoint
                           , SDFocusPoints
                           , ServerData(..)
                           )
import Control.Monad.Except (Except, except, runExcept, mapExcept, throwError)
import Data.DataFrame (DataFrame(..))
import Data.DataFrame as DF
import Data.Either (Either(..), either)
import Data.Foldable (class Foldable, foldMap, foldr, minimumBy)
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
type DataPoints d = DataFrame (DataPoint d)
type FieldNames d = Array String
type CurvePoint = { x1Min :: Number, x1Max :: Number
                  , x2Min :: Number, x2Max :: Number 
                  , focusPointId :: Int
                  }
type CurvePoints = DataFrame CurvePoint
--type FocusPoints2D = { group :: Int, data :: DataFrame CurvePoint }
type Dim2D = Tuple Int Int
type Dims2D = { group :: Dim2D, data :: CurvePoints }
type SliceData = DataFrame Dims2D
type LocalCurve d = DataRow {fp :: Point d, curves :: Maybe SliceData}
type LocalCurves d = DataFrame (LocalCurve d)
type Point2D = DataRow (Array Number) -- 2 numbers!

derive instance newtypeDataRow :: Newtype (DataRow a) _

instance functorDataRow :: Functor DataRow where
  map f (DataRow r) = DataRow $ r {row = f r.row}

instance applyDataRow :: Apply DataRow where
  apply (DataRow f) (DataRow r) = DataRow $ f {row = f.row r.row}

rowVal :: forall a. DataRow a -> a
rowVal (DataRow r) = r.row

rowId :: forall a. DataRow a -> Int
rowId (DataRow r) = r.rowId

rowInit :: forall a. a -> Int -> DataRow a
rowInit x i = DataRow {rowId: i, row: x}

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

closestPoint :: forall d. DataPoints d -> Point d -> Maybe (DataPoint d)
closestPoint dps pt = minimumBy ord dps
  where
  ord p1 p2 = P.distOrd pt (rowVal p1) (rowVal p2)

closestPoint2d :: forall d. Int -> Int -> DataPoints d -> Point d -> Maybe (DataPoint d)
closestPoint2d d1 d2 dps pt = minimumBy ord dps
  where
  ord p1 p2 = P.distOrd (P.project2D d1 d2 pt) 
                        (P.project2D d1 d2 (rowVal p1))
                        (P.project2D d1 d2 (rowVal p2))

ptsFromServerData :: forall d
                   . SDPoints 
                  -> Except String (Tuple (FieldNames d) (DataPoints d))
ptsFromServerData pts = do
  r1 <- withFail "No data" $ A.head pts
  let fields = A.sort $ SM.keys r1
      --ids = 1..(A.length pts)
  pts' :: Array (Point d) <- for pts $ \pt -> do
    ptArray :: Array Number <- traverse (lookupField pt) fields
    withFail "Invalid point size" $ P.fromArray ptArray
  pure $ Tuple fields (DF.init $ a2dr pts')

fpsFromServerData :: forall d
                   . SDFocusPoints
                  -> Except String (DataPoints d)
fpsFromServerData fps = do
  fps' <- for fps $ \fp ->
    withFail "Invalid focus point size" $ P.fromArray fp
  pure $ DF.init (a2dr fps')

curvesFromServerData :: SDCurves -> DataFrame SDCurve
curvesFromServerData = DF.init

lookupField :: StrMap Number -> String -> Except String Number
lookupField m f = withFail ("field " <> f <> " missing") $ SM.lookup f m

a2dr :: forall a. Array a -> Array (DataRow a)
a2dr xs = A.zipWith rowInit xs ids
  where
  ids = 1..(A.length xs)

{--lookupPoint :: forall d. ParetoPoints d -> Int -> Except String (DataPoint d)--}
{--lookupPoint (DataFrame pts) i = --}
  {--withFail ("row " <> (show i) <> " not found") $ pts !! (i-1)--}

withFail :: forall a. String -> Maybe a -> Except String a
withFail msg = maybe (throwError msg) pure

