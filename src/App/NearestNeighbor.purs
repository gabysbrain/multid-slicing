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

