
module App.NearestNeighbor where

import Prelude
import App.Data (AppData, AppDatum)
import Pareto (ParetoSlabs, pointSqDist)
import Data.List (List)
import Data.List as L
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.DataFrame as DF

-- TODO: move the data types around to unify them...
radialNN :: Number -> AppData -> ParetoSlabs
radialNN r df = DF.init $ 
              map (\x -> {slab:x.nbrId, data:DF.init x.nbrs}) $
              radialNN_ r (L.fromFoldable df) L.Nil

radialNN_ :: Number 
          -> List AppDatum 
          -> List (List AppDatum)
          -> List {nbrId :: Int, nbrs :: List AppDatum}
radialNN_ r df accum = case L.uncons df of
  Nothing -> L.mapWithIndex (\ns i -> {nbrId:i, nbrs:ns}) accum
  Just {head:pt,tail:pts} -> radialNN_ r pts (accum <> createNbrs r pt pts)

createNbrs :: Number -> AppDatum -> List AppDatum -> List (List AppDatum)
createNbrs r pt = foldMap (mkNbr pt)
  where
  mkNbr p1 p2 = if fromMaybe false $ (>) r <$> pointSqDist p1 p2
                   then L.singleton (p1 L.: p2 L.: L.Nil)
                   else L.Nil


