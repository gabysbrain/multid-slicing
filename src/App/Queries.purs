module App.Queries where

import Prelude
import App.Data (ParetoPoints, DataPoint, Link, AngleLink, Node, NeighborGraph, PointData2D, LineData2D)
import App.NearestNeighbor (radialNN)
import Data.Array as A
import Data.DataFrame (DataFrame, Query)
import Data.DataFrame as DF
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.StrMap as SM
import Data.Tuple (Tuple(..), uncurry)
import Math (atan, sqrt)
import Pareto (ParetoSlab, ParetoSlabs, pareto2dSlabs)
import Data.Geom.Point ((!!!))
import Data.Geom.Point as P
import Data.Geom.Vector as V

----------------------------------
-- All the queries the app uses --
----------------------------------
graphLinks :: forall d. Query (NeighborGraph d) (DataFrame (Link d))
graphLinks = do -- FIXME: why doesn't map work here?
  g <- DF.reset
  pure g.links

graphNodes :: forall d. Query (NeighborGraph d) (DataFrame (Node d))
graphNodes = do
  g <- DF.reset
  pure g.nodes

-- radius-based neighborhood calculation
nbrs :: forall d. Number -> Query (ParetoPoints d) (NeighborGraph d)
nbrs r = do
  nodes <- DF.reset
  let links = DF.init $ radialNN r nodes
  pure $ {nodes:nodes, links:links}

limits2d :: forall d
          . Int -> Int 
         -> Query (ParetoPoints d) (Tuple Number Number)
limits2d d1 d2 = max2d <$> points2d
  where
  points2d = DF.summarize (extract2d d1 d2)

scatterplotPoints :: forall d
                   . Set Int -> Int -> Int
                  -> Query (ParetoPoints d) (Array PointData2D)
scatterplotPoints highlightPts d1 d2 = 
  map (setHighlight highlightPts) <$>
  DF.summarize (extract2dPt d1 d2)

paretoPlotPaths :: forall d
                 . Number -> Set Int -> Int -> Int
                -> Query (DataFrame (Link d)) (Array LineData2D)
paretoPlotPaths r highlightFronts d1 d2 =
  --pareto2dSlabs r d1 d2 `DF.chain`
  linkAngle2d d1 d2 `DF.chain`
  DF.summarize (extractPath' highlightFronts d1 d2)

linkAngle2d :: forall d
             . Int -> Int 
            -> Query (DataFrame (Link d)) (DataFrame (AngleLink d))
linkAngle2d d1 d2 = DF.mutate angleLink
  where
  angleLink l = 
    { cosTheta: cosTheta2d d1 d2 l
    , src: l.src
    , tgt: l.tgt
    , linkId: l.linkId
    }

-- sort the points so that the line drawing algorithm works correctly
{--paretoSort :: String -> String -> Query ParetoSlabs ParetoSlabs--}
{--paretoSort d1 d2 = DF.mutate innerSort'--}
  {--where--}
  {--innerSort' {slab:s, data:d} = --}
    {--{ slab: s--}
    {--, data: DF.runQuery (DF.sort (order2d d1 d2)) d--}
    {--}--}


-------------------------------------------
-- Utility functions used by the queries --
-------------------------------------------
extract2d :: forall d
           . Int -> Int -> DataPoint d -> Tuple Number Number
extract2d d1 d2 {point:d} = Tuple (d !!! d1) (d !!! d2)

max2d :: Array (Tuple Number Number) -> Tuple Number Number
max2d = foldl max' (Tuple 0.0 0.0)
  where
  max' (Tuple x1 y1) (Tuple x2 y2) = Tuple (max x1 x2) (max y1 y2)

extractPath :: forall d. Set Int -> Int -> Int -> ParetoSlab d -> LineData2D
extractPath selIds d1 d2 {slab:g, p1:p1, p2:p2} = 
  { slabId: g
  , selected: Set.member g selIds
  , points: [extract2dPt d1 d2 p1, extract2dPt d1 d2 p2]
  , cosTheta: 1.0
  }

extractPath' :: forall d. Set Int -> Int -> Int -> AngleLink d -> LineData2D
extractPath' selIds d1 d2 link =
  { slabId: link.linkId
  , selected: Set.member link.linkId selIds
  , points: [extract2dPt d1 d2 link.src, extract2dPt d1 d2 link.tgt]
  , cosTheta: link.cosTheta
  }

extract2dPt :: forall d. Int -> Int -> DataPoint d -> PointData2D
extract2dPt d1 d2 datum = 
  { rowId: datum.rowId
  , x: datum.point !!! d1
  , y: datum.point !!! d2
  , selected: false
  }

setHighlight :: Set Int -> PointData2D -> PointData2D
setHighlight highlightPts pt = pt {selected=Set.member pt.rowId highlightPts}

cosTheta2d :: forall d. Int -> Int -> Link d -> Number
cosTheta2d d1 d2 {src:{point:p1},tgt:{point:p2}} = sqrt (V.sqLen v' / V.sqLen v)
  where
  v = V.fromPoints p1 p2
  v' = V.fromPoints (P.project d1 d2 p1) (P.project d1 d2 p2)

{--order2d :: String -> String -> AppDatum -> AppDatum -> Ordering--}
{--order2d d1 d2 pt1 pt2 = --}
  {--fromMaybe EQ $ compare <$> (pt2theta d1 d2 pt1) <*> (pt2theta d1 d2 pt2)--}

{--pt2theta :: String -> String -> AppDatum -> Maybe Number--}
{--pt2theta d1 d2 {point:pt} = do--}
  {--x <- SM.lookup d1 pt--}
  {--y <- SM.lookup d2 pt--}
  {--pure $ atan (y/x)--}

