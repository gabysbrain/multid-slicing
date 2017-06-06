module App.Queries where

import Prelude
import App.Data (AppData, AppDatum, Link, AngleLink, Node, NeighborGraph, PointData, LineData)
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
import Util (joinWith)

----------------------------------
-- All the queries the app uses --
----------------------------------
graphLinks :: Query NeighborGraph (DataFrame Link)
graphLinks = do -- FIXME: why doesn't map work here?
  g <- DF.reset
  pure g.links

graphNodes :: Query NeighborGraph (DataFrame Node)
graphNodes = do
  g <- DF.reset
  pure g.nodes

-- radius-based neighborhood calculation
nbrs :: Number -> Query AppData NeighborGraph
nbrs r = do
  nodes <- DF.reset
  let links = DF.init $ radialNN r nodes
  pure $ {nodes:nodes, links:links}

limits2d :: String -> String -> Query AppData (Tuple Number Number)
limits2d d1 d2 = max2d <$> points2d
  where
  points2d = DF.summarize (extract2d d1 d2)

scatterplotPoints :: Set Int -> String -> String
                  -> Query AppData (Array PointData)
scatterplotPoints highlightPts d1 d2 = 
  map (setHighlight highlightPts) <$>
  A.catMaybes <$>
  DF.summarize (extract2dPt d1 d2)

paretoPlotPaths :: Number -> Set Int -> String -> String
                -> Query (DataFrame Link) (Array LineData)
paretoPlotPaths r highlightFronts d1 d2 =
  --pareto2dSlabs r d1 d2 `DF.chain`
  linkAngle2d d1 d2 `DF.chain`
  DF.summarize (extractPath' highlightFronts d1 d2)

linkAngle2d :: String -> String -> Query (DataFrame Link) (DataFrame AngleLink)
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
extract2d :: String -> String -> AppDatum -> Maybe (Tuple Number Number)
extract2d d1 d2 {point:d} = do
  v1 <- SM.lookup d1 d
  v2 <- SM.lookup d2 d
  pure $ Tuple v1 v2

max2d :: Array (Maybe (Tuple Number Number)) -> Tuple Number Number
max2d = foldl max' (Tuple 0.0 0.0)
  where
  max' :: Tuple Number Number -> Maybe (Tuple Number Number) -> Tuple Number Number
  max' t Nothing = t
  max' (Tuple x1 y1) (Just (Tuple x2 y2)) = Tuple (max x1 x2) (max y1 y2)

extractPath :: Set Int -> String -> String -> ParetoSlab -> LineData
extractPath selIds d1 d2 {slab:g, p1:p1, p2:p2} = 
  { slabId: g
  , selected: Set.member g selIds
  , points: A.catMaybes $ [extract2dPt d1 d2 p1, extract2dPt d1 d2 p2]
  , cosTheta: 1.0
  }

extractPath' :: Set Int -> String -> String -> AngleLink -> LineData
extractPath' selIds d1 d2 link =
  { slabId: link.linkId
  , selected: Set.member link.linkId selIds
  , points: A.catMaybes $ [extract2dPt d1 d2 link.src, extract2dPt d1 d2 link.tgt]
  , cosTheta: link.cosTheta
  }

extract2dPt :: String -> String -> AppDatum -> Maybe PointData
extract2dPt d1 d2 datum = do
  v1 <- SM.lookup d1 datum.point
  v2 <- SM.lookup d2 datum.point
  pure $ {rowId:datum.rowId, x:v1, y:v2, selected: false}

setHighlight :: Set Int -> PointData -> PointData
setHighlight highlightPts pt = pt {selected=Set.member pt.rowId highlightPts}

tmp d1 d2 p = do
  v1 <- SM.lookup d1 p
  v2 <- SM.lookup d2 p
  pure $ Tuple v1 v2

cosTheta2d :: String -> String -> Link -> Number
cosTheta2d d1 d2 {src:{point:p1},tgt:{point:p2}} = sqrt (sqLen' / sqLen)
  where
  p = joinWith ((-)) p1 p2
  p' = fromMaybe (Tuple 0.0 0.0) $ tmp d1 d2 p -- projected line
  sqLen = foldl (\s v -> s + v*v) 0.0 $ SM.values p
  sqLen' = uncurry (\x1 x2 -> x1*x1 + x2*x2) p'

{--order2d :: String -> String -> AppDatum -> AppDatum -> Ordering--}
{--order2d d1 d2 pt1 pt2 = --}
  {--fromMaybe EQ $ compare <$> (pt2theta d1 d2 pt1) <*> (pt2theta d1 d2 pt2)--}

{--pt2theta :: String -> String -> AppDatum -> Maybe Number--}
{--pt2theta d1 d2 {point:pt} = do--}
  {--x <- SM.lookup d1 pt--}
  {--y <- SM.lookup d2 pt--}
  {--pure $ atan (y/x)--}

