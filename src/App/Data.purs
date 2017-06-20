module App.Data where

import Prelude 
import Control.Monad.Except (Except, except, runExcept, mapExcept, throwError)
import Data.DataFrame (DataFrame)
import Data.DataFrame as DF
import Data.Either (Either(..), either)
import Data.Foldable (class Foldable, foldMap, foldr)
import Data.Array as A
import Data.List (List)
import Data.List as L
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Monoid (class Monoid, mempty)
import Data.Number (fromString)
import Data.String (Pattern(..), split, trim)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Formatter.Number (format)
import Data.Geom.Point (Point)
import Data.Geom.Point as P

newtype DataRow a = DataRow {rowId :: Int, row :: a}
type DataPoint d = DataRow (Point d)
type FieldNames d = Array String
type RawPoints d = DataFrame (DataPoint d)
type ParetoPoints d = DataFrame (DataPoint d)

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

data CsvError 
  = NoHeaderRow
  | NoDataRows
  | InvalidHeaderFieldCount
  | InvalidFieldCount {row :: Int, actual :: Int}
  | ConvertErr { row :: Int
               , col :: Int
               , message :: String
               }
instance showCsvError :: Show CsvError where
  show NoHeaderRow = "No header row"
  show NoDataRows = "No data rows"
  show InvalidHeaderFieldCount = "Wrong number of header fields"
  show (InvalidFieldCount e) = 
       "(row: " <> (show e.row) <> ") " 
    <> "incorrect length"
    -- <> (if e.expected < e.actual then "row too long" else "row too short") 
    -- <> " expected: " <> (show e.expected) 
    <> " actual: " <> (show e.actual)
  show (ConvertErr e) = "(row: " <> (show e.row) 
                     <> " col: " <> (show e.col) <> ") " 
                     <> e.message

type CE = Except (NonEmptyList CsvError)

fromCsv :: forall d. String -> CE (Tuple (FieldNames d) (RawPoints d))
fromCsv raw = case L.uncons $ splitLines raw of
    Nothing -> throwError $ pure NoHeaderRow
    Just {tail:t} | L.length t == 0 -> throwError $ pure NoDataRows
    Just {head:h,tail:t} -> do
      fields <- parseFieldNames' h
      dataRows <- withRowIds <$> fromCsv' t
      pure $ Tuple fields (DF.init dataRows)
                 -- mapExcept (either Left (Right <<< Tuple (parseFieldNames' h) <<< DF.init)) $ 
                              -- withRowIds <$>
                              -- fromCsv' t
                              --fromCsv' (split' (Pattern ",") h) t

-- TODO: I can put the header length here to force all the parsing to normalize
fromCsv' :: forall d. List String -> CE (List (Point d))
fromCsv' lines = mergeErrs $
  L.zipWith (\i l -> mapExcept (mapPts i) $ procLine l)
            (L.range 1 (L.length lines))
            lines
  where
  mapPts i =
    either (\errs -> Left (map (\e -> ConvertErr {row:i,col:fst e,message:snd e}) errs))
           (\vals -> maybe (Left (pure (InvalidFieldCount {row:i,actual:L.length vals})))
                           (Right <<< pure) $
                           toPoint vals)

parseFieldNames' :: forall d. String -> CE (FieldNames d)
parseFieldNames' = parseFieldNames <<< split' (Pattern ",")

withRowIds :: forall d. List (Point d) -> List (DataPoint d)
withRowIds = L.mapWithIndex (\i p -> DataRow {rowId:i, row:p})

parseFieldNames :: forall d. List String -> CE (FieldNames d)
parseFieldNames = pure <<< A.fromFoldable
  -- version for sized vectors
  --maybe (throwError $ pure InvalidHeaderFieldCount)
        --pure <<< V.fromArray <<< A.fromFoldable 

splitLines :: String -> List String
splitLines raw = L.filter ((/=) "") $ map trim $ split' (Pattern "\n") raw

procLine :: String -> Except (NonEmptyList (Tuple Int String)) (List Number)
procLine line = mergeErrs $ 
  L.zipWith (\i f -> mapExcept (mapExceptions i) $ convertField f)
            (L.range 1 (L.length fields))
            fields
  where
  fields = L.fromFoldable $ split (Pattern ",") line
  mapExceptions i = 
    either (Left <<< pure <<< Tuple i) (Right <<< L.singleton)
    --(L.singleton <<< either (Left <<< pure <<< Tuple i) (Right <<< L.singleton))

convertField :: String -> Except String Number
convertField val = 
  maybe (throwError $ "cannot convert '" <> val <> "' to double") pure $ fromString val

split' p = map trim <<< L.fromFoldable <<< split p

mergeErrs :: forall f n e. Semigroup e => Monoid n => Foldable f => 
             f (Except e n) -> Except e n
mergeErrs = foldr (\e1 e2 -> except $ merge' (runExcept e1) (runExcept e2))
                  (pure mempty)

toPoint :: forall d. List Number -> Maybe (Point d)
toPoint = P.fromArray <<< A.fromFoldable

merge' :: forall m n. Semigroup m => Semigroup n => Either m n -> Either m n -> Either m n
merge' (Left e1) (Left e2) = Left $ e1 <> e2
merge' (Right _) (Left e2) = Left e2
merge' (Left e1) (Right _) = Left e1
merge' (Right v1) (Right v2) = Right $ v1 <> v2

