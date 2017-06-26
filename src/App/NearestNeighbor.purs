module App.NearestNeighbor 
  where
  --( radialNN
  --) where

import Prelude
import App.Data (DataPoint, Link, rowVal)
import Data.Bifunctor (bimap)
import Data.List (List)
import Data.List as L
import Data.Foldable (class Foldable, foldl, foldMap, maximumBy, minimumBy)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Tuple (Tuple(..), fst, snd, swap)
import Data.Geom.Point (Point)
import Data.Geom.Point as P
import Data.Geom.Point ((!!!))
import Data.Geom.Vector as V
import Data.Geom.Line as Line
import Data.Geom.Simplex (Facet, Simplex)
import Data.Geom.Simplex as S
import Partial.Unsafe (unsafePartial)

newtype HullSegment d = HullSegment (Tuple (Point d) (Point d))

instance showHullSegment :: Show (HullSegment d) where
  show (HullSegment hs) = show hs

-- equality is equal in both directions. we might need to fix this later...
instance eqHullSegment :: Eq (HullSegment d) where
  eq (HullSegment hs1) (HullSegment hs2) = (hs1 == hs2) || (swap hs1 == hs2)

mapPts :: forall d d'. (Point d -> Point d') -> HullSegment d -> HullSegment d'
mapPts f (HullSegment hs) = HullSegment $ bimap f f hs

-- TODO: move the data types around to unify them...
-- | neighborhood based on radius
radialNN :: forall f d. Foldable f 
         => Number -> f (DataPoint d) -> List (Link d)
radialNN r df = _radialNN r (L.fromFoldable df) L.Nil

_radialNN :: forall d
           . Number 
          -> List (DataPoint d)
          -> List {src::DataPoint d,tgt::DataPoint d}
          -> List (Link d)
_radialNN r df accum = case L.uncons df of
  Nothing -> L.mapWithIndex (\i ns -> {linkId:i,src:ns.src,tgt:ns.tgt}) accum
  Just {head:pt,tail:pts} -> _radialNN r pts (accum <> createNbrsR r pt pts)

-- find the set of neighbors a certain distance around a point (pt)
createNbrsR :: forall d
             . Number -> DataPoint d -> List (DataPoint d) 
            -> List {src::DataPoint d,tgt::DataPoint d}
createNbrsR r pt = foldMap (mkNbr pt)
  where
  mkNbr p1 p2 = if rowVal (P.sqDist <$> p1 <*> p2) <= r*r
                   then L.singleton {src:p1, tgt:p2}
                   else L.Nil

-- | delauny triangulation computation
delauny :: forall f d. Foldable f
        => f (Point d) -> List (HullSegment d)
--      => f (DataPoint d) -> List (HullSegment d)
delauny = projectUp >>> convexHull >>> extractSegments >>> L.filter lowerLink >>> projectDown
  where
  projectUp = foldMap (pure <<< addDistDim)
  projectDown = map (mapPts rmDistDim)

addDistDim :: forall d d'. Point d -> Point d'
addDistDim = P.projectUp (pure <<< distFun)
  where
  distFun = foldl (\s x -> s + x*x) 0.0

rmDistDim :: forall d d'. Point d -> Point d'
rmDistDim p = P.projectNot (P.dims p - 1) p

-- determine if a link is part of the "lower" hull
lowerLink :: forall d. HullSegment d -> Boolean
lowerLink (HullSegment link) = 
    (fst link !!! lastidx) - (snd link !!! lastidx) /= 0.0
  where
  lastidx = P.dims $ fst link

extractSegments :: forall d. List (Facet d) -> List (HullSegment d)
extractSegments = map HullSegment <<< L.nub <<< foldMap S.ridges

convexHull :: forall d. List (Point d) -> List (Facet d)
convexHull = quickhull

quickhull :: forall d. List (Point d) -> List (Facet d)
quickhull L.Nil = L.Nil -- no points do not a hull make
quickhull l@(L.Cons p1 _) | L.length l < (P.dims p1) + 1 = L.Nil -- not enough pts
quickhull pts = foldMap (\f -> findHull f (ptsAbove f pts')) $ S.facets simplex
  where
  Tuple simplex pts' = initialSimplex pts

-- TODO: start with something better, esp. if points are colinear...
initialSimplex :: forall d. List (Point d) -> Tuple (Simplex d) (List (Point d))
initialSimplex pts = Tuple (S.fromPoints (L.take (dims+1) pts)) 
                           (L.drop (dims+1) pts)
  where
  dims = maybe 0 P.dims $ L.head pts

ptsAbove :: forall d. Facet d -> List (Point d) -> List (Point d)
ptsAbove facet = L.filter (S.aboveFacet facet)

maxPt :: forall d. Facet d -> List (Point d) -> Point d
maxPt facet = unsafeMaxBy (\p1 p2 -> compare (S.facetDist facet p1) (S.facetDist facet p2))

-- TODO: make the foldMap part cleaner (and above)
findHull :: forall d. Facet d -> List (Point d) -> List (Facet d)
findHull facet L.Nil = pure facet
findHull facet pts = foldMap (\f -> findHull f (ptsAbove f pts')) facets
  where
  pt = maxPt facet pts
  pts' = L.delete pt pts
  s' = S.fromFacet facet pt
  facets = L.delete facet $ S.facets s'

-- dangerous functions but these are only run on non-empty lists
unsafeMaxBy :: forall a f. Foldable f => (a -> a -> Ordering) -> f a -> a
unsafeMaxBy f l = unsafePartial $ fromJust $ maximumBy f l
unsafeMinBy :: forall a f. Foldable f => (a -> a -> Ordering) -> f a -> a
unsafeMinBy f l = unsafePartial $ fromJust $ minimumBy f l

