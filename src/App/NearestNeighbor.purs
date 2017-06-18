
module App.NearestNeighbor where

import Prelude
import App.Data (DataPoint, Link)
import Data.List (List)
import Data.List as L
import Data.Foldable (class Foldable, foldMap)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Geom.Point as P

-- TODO: move the data types around to unify them...
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
  Just {head:pt,tail:pts} -> _radialNN r pts (accum <> createNbrs r pt pts)

-- find the set of neighbors a certain distance around a point (pt)
createNbrs :: forall d
            . Number -> DataPoint d -> List (DataPoint d) 
           -> List {src::DataPoint d,tgt::DataPoint d}
createNbrs r pt = foldMap (mkNbr pt)
  where
  mkNbr p1 p2 = if r*r > P.sqDist p1.point p2.point
                   then L.singleton {src:p1, tgt:p2}
                   else L.Nil

