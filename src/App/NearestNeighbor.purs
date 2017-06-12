
module App.NearestNeighbor where

import Prelude
import App.Data (AppData, AppDatum, Link)
import Pareto (ParetoSlabs, pointSqDist)
import Data.List (List)
import Data.List as L
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.DataFrame as DF

-- TODO: move the data types around to unify them...
radialNN :: Number -> AppData -> List Link
radialNN r df = _radialNN r (L.fromFoldable df) L.Nil

_radialNN :: Number 
          -> List AppDatum 
          -> List {src::AppDatum,tgt::AppDatum}
          -> List Link
_radialNN r df accum = case L.uncons df of
  Nothing -> L.mapWithIndex (\i ns -> {linkId:i,src:ns.src,tgt:ns.tgt}) accum
  Just {head:pt,tail:pts} -> _radialNN r pts (accum <> createNbrs r pt pts)

-- find the set of neighbors a certain distance around a point (pt)
createNbrs :: Number -> AppDatum -> List AppDatum -> List {src::AppDatum,tgt::AppDatum}
createNbrs r pt = foldMap (mkNbr pt)
  where
  mkNbr p1 p2 = if fromMaybe false $ (>) r <$> pointSqDist p1 p2
                   then L.singleton {src:p1, tgt:p2}
                   else L.Nil

