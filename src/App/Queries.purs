module App.Queries where

import Prelude
import App.Data ( DataPoints, DataPoint, CurvePoint, CurvePoints
                , SliceData, Dim2D, Dims2D, LocalCurve, LocalCurves, Point2D
                , rowId, rowVal, a2dr )
import App.Data.ServerData (SDCurve(..))
import Data.Array as A
import Data.DataFrame (DataFrame, Query)
import Data.DataFrame as DF
import Data.Foldable (foldl, foldMap)
import Data.Maybe (Maybe(Nothing), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Math (sqrt)
import Data.Geom.Point ((!!!))
import Data.Geom.Point as P
import Data.Geom.Vector as V

----------------------------------
-- All the queries the app uses --
----------------------------------

type RawCurve = DataFrame SDCurve
type Dims2DGrouped = DataFrame {group::Dim2D, data::RawCurve}

internalizeData :: Query RawCurve SliceData
internalizeData = group2D `DF.chain` _2dFilter

curve2dFilter :: Int -> Int -> Query SliceData (Array CurvePoint)
curve2dFilter d1 d2 = do
  d <- DF.filter (\r -> r.group == Tuple (d1+1) (d2+1)) `DF.chain`
       DF.summarize (\r -> DF.runQuery (DF.summarize id) r.data)
  pure $ A.concat d

fp2dFilter :: forall d. Int -> Int -> Query (DataPoints d) (Array Point2D)
fp2dFilter d1 d2 = DF.summarize f
  where
  f = map (P.toArray <<< P.project2D d1 d2)

fpFilter :: Int -> Query SliceData SliceData
fpFilter fpId = 
    DF.mutate (\r -> r {data=DF.runQuery dataFilter r.data})
  where 
  dataFilter = DF.filter (\rr -> rr.focusPointId==fpId)

lc2c2d :: forall d. Int -> Int -> Query (LocalCurves d) (Array CurvePoint)
lc2c2d d1 d2 = do
  curves <- DF.summarize (localCurve2curve2d d1 d2)
  pure $ A.concat curves

lc2fp :: forall d. Query (LocalCurves d) (DataPoints d)
lc2fp = DF.mutate (map (\r -> r.fp))

localFp :: forall d. Int -> Query (DataPoints d) (LocalCurves d)
localFp fpId = filterFps fpId `DF.chain` fps2lcs

-------------------------------------------
-- Utility functions used by the queries --
-------------------------------------------
extract2d :: forall d
           . Int -> Int -> DataPoint d -> Tuple Number Number
extract2d d1 d2 p = rowVal $ map (\p' -> Tuple (p' !!! d1) (p' !!! d2)) p

max2d :: Array (Tuple Number Number) -> Tuple Number Number
max2d = foldl max' (Tuple 0.0 0.0)
  where
  max' (Tuple x1 y1) (Tuple x2 y2) = Tuple (max x1 x2) (max y1 y2)

localCurve2curve2d :: forall d. Int -> Int -> LocalCurve d -> Array CurvePoint
localCurve2curve2d d1 d2 lc = 
  forceFpId (rowId lc) $ DF.runQuery (curve2dFilter d1 d2) $ changeLC lc

changeLC :: forall d. LocalCurve d -> SliceData
changeLC lc = fromMaybe (DF.init []) (rowVal lc).curves

forceFpId :: Int -> Array CurvePoint -> Array CurvePoint
forceFpId fpid = map (\r -> r {focusPointId=fpid})

_2dFilter :: Query Dims2DGrouped SliceData
_2dFilter = DF.mutate f
  where 
  f :: {group::Dim2D, data::RawCurve} -> Dims2D
  f r = r {data=DF.runQuery filterCurvePoints r.data}

group2D :: Query RawCurve Dims2DGrouped
group2D = DF.group f
  where f (SDCurve c) = Tuple c.d1 c.d2

filterCurvePoints :: Query RawCurve (DataFrame CurvePoint)
filterCurvePoints = DF.mutate f
  where f (SDCurve c) = { x1Min: c.x1Start, x1Max: c.x1End
                        , x2Min: c.x2Start, x2Max: c.x2End 
                        , focusPointId: c.fpid
                        }

filterFps :: forall d. Int -> Query (DataPoints d) (DataPoints d)
filterFps fpId = DF.filter (\rr -> rowId rr==fpId)

fps2lcs :: forall d. Query (DataPoints d) (LocalCurves d)
fps2lcs = do
  fps <- DF.summarize (\r -> {fp: rowVal r, curves: Nothing})
  pure $ DF.init (a2dr fps)


{--setHighlight :: Set Int -> PointData2D -> PointData2D--}
{--setHighlight highlightPts pt = pt {selected=Set.member pt.rowId highlightPts}--}

{--cosTheta2d :: forall d. Int -> Int -> Link d -> Number--}
{--cosTheta2d d1 d2 {src:p1,tgt:p2} = sqrt (V.sqLen v' / V.sqLen v)--}
  {--where--}
  {--p1' = P.project2D d1 d2 <$> p1--}
  {--p2' = P.project2D d1 d2 <$> p2--}
  {--v = rowVal $ V.fromPoints <$> p1 <*> p2--}
  {--v' = rowVal $ V.fromPoints <$> p1' <*> p2'--}

