module Pareto where

import Prelude

import App.Data (DataPoint, RawPoints, ParetoPoints, Link, rowId, rowVal)
import Data.DataFrame (DataFrame, Query)
import Data.DataFrame as DF
import Data.Array as A
import Data.Foldable (class Foldable, or, foldMap, any)
import Data.List as L
import Data.Maybe (fromJust)
import Data.Ordering (invert)
import Partial.Unsafe (unsafePartial)
import Data.Geom.Point as P

type ParetoSlab d = {slab :: Int, p1 :: DataPoint d, p2 :: DataPoint d}
type ParetoSlabs d = DF.DataFrame (ParetoSlab d)

paretoSet :: forall d. Query (RawPoints d) (ParetoPoints d)
paretoSet = paretoSubset id

paretoSubset :: forall d. (DataPoint d -> DataPoint d) 
             -> Query (RawPoints d) (ParetoPoints d)
paretoSubset dimFilter = DF.init <$> _paretoSet dimFilter

pareto2dSlabs :: forall d. Number -> Int -> Int 
              -> Query (DataFrame (Link d)) (ParetoSlabs d)
pareto2dSlabs r d1 d2 = 
    DF.filter (\l -> radiusFilter l.src l.tgt) `DF.chain`
    DF.mutate link2slab
  where
  radiusFilter p1 p2 = (r*r) < (pointSqDist (filterNotDatum2D d1 d2 p1) 
                                            (filterNotDatum2D d1 d2 p2))
  link2slab l = {slab:l.linkId, p1:l.src, p2: l.tgt}
  
-- FIXME: not sure why I need this separate function but ok...
-- TODO: figure out the better way to handle the monad wrt 
--       why I can't use whileM. Maybe switch Query to a state Monad...
_paretoSet :: forall d
            . (DataPoint d -> DataPoint d) 
           -> Query (RawPoints d) (L.List (DataPoint d))
_paretoSet dimFilter = do
  nonempty <- nonemptyDF
  if nonempty
     then do
       -- TODO: put all dim filtering in here and remove it from the 
       --       processing functions
       p <- rowOne <$> DF.sort (paretoOrder dimFilter) `DF.chain` DF.trim 1 
       --DF.filter (not <<< comparable (rowOne p))
       --fds <- DF.filter (pointFilter dimFilter p)
       cons p <$> (DF.filter (pointFilter dimFilter p) `DF.chain` _paretoSet dimFilter)
       --pure $ p L.: (DF.runQuery (_paretoSet dimFilter) fds)
     else pure L.Nil

paretoOrder :: forall d
             . (DataPoint d -> DataPoint d) 
             -> DataPoint d -> DataPoint d -> Ordering
paretoOrder dimFilter p1 p2 = invert $
  if p1' < p2' then LT
  else if p1' > p2' then GT
  else EQ
  where 
  p1' = sumPt $ dimFilter p1
  p2' = sumPt $ dimFilter p2

sumPt :: forall d. DataPoint d -> Number
sumPt pt = rowVal $ P.fold ((+)) 0.0 <$> pt

-- determine if p1 is comparable to p2
-- i.e. at least one factor of p2 >= p1
comparable :: forall d. DataPoint d -> DataPoint d -> Boolean
comparable p1 p2 = or $ rowVal $ P.zipDimsWith ((<=)) <$> p1 <*> p2

pointSqDist :: forall d. DataPoint d -> DataPoint d -> Number
pointSqDist p1 p2 = rowVal $ P.sqDist <$> p1 <*> p2

pointDistCmp :: forall d. DataPoint d -> DataPoint d -> Ordering
pointDistCmp p1 p2 = invert $ compare d1 d2
  where 
  d1 = pointSqDist p1 p1
  d2 = pointSqDist p2 p2

pointFilter :: forall d
             . (DataPoint d -> DataPoint d) 
            -> DataPoint d -> DataPoint d -> Boolean
pointFilter dimFilter p1 p2 = (rowId p1 /= rowId p2) && (comparable p1' p2')
  where
  p1' = dimFilter p1
  p2' = dimFilter p2

nonemptyDF :: forall d. Query (RawPoints d) Boolean
nonemptyDF = do
  rs <- A.length <$> DF.summarize id
  pure $ rs /= 0

rowOne :: forall d. RawPoints d -> DataPoint d
rowOne = unsafePartial $ fromJust <<< L.head <<< foldMap L.singleton
--rowOne = fromMaybe SM.empty <<< L.head <<< foldMap L.singleton

filterNotDatum2D :: forall d d'
                  . Int -> Int -> DataPoint d -> DataPoint d'
filterNotDatum2D d1 d2 datum = P.projectNot2D d1 d2 <$> datum

-- Filter a point to 2D
filterDatum2D :: forall d d'. Int -> Int -> DataPoint d -> DataPoint d'
filterDatum2D d1 d2 datum = P.project2D d1 d2 <$> datum

notElemBy :: forall a f. Foldable f => (a -> a -> Boolean) -> a -> f a -> Boolean
notElemBy f x = not <<< (elemBy f x)

elemBy :: forall a f. Foldable f => (a -> a -> Boolean) -> a -> f a -> Boolean
elemBy f = any <<< f

cons :: forall a. a -> L.List a -> L.List a
cons x xs = x L.: xs

