module App.View.ParetoSlices where

import Prelude hiding (div)
import App.Data (AppData, AppDatum, PointData, LineData, NeighborGraph, sortedFieldNames)
import App.Events (Event)
import App.State (State(..), DataInfo)
import App.Queries (graphNodes, graphLinks, limits2d, nbrs, scatterplotPoints, paretoPlotPaths)
import App.View.ParetoVis as PV
import Data.Array as A
import Data.DataFrame as DF
import Data.DataFrame (Query)
import Data.Foldable (foldl, foldMap)
import Data.List (List)
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.StrMap as SM
import Data.Tuple (Tuple(..), fst, snd)
import Data.Traversable (for_)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (div, label, h2, h3, button, input, span, ul, li, p)
import Text.Smolder.HTML.Attributes (className, type')
import Text.Smolder.Markup ((!), (#!), text)

view :: DataInfo -> HTML Event
view dsi = div $ 
  div ! className "splom-view" $ do
    div ! className "splom dims x-axis" $ do
      -- labels for x-axes
      div $ pure unit -- empty cell to offset the axis labels
      for_ (fromMaybe L.Nil $ L.init $ sortedFieldNames dsi.paretoPoints) $ \fn -> do
        label ! className "dim-label" $ text fn
    for_ (L.transpose $ map L.reverse $ splomPairs $ sortedFieldNames dsi.paretoPoints) $ \sr -> do
      div ! className "splom row" $ do
        --div ! className "splom dims y-axis" $ 
        label ! className "dim-label" $ text $ fromMaybe "" $ snd <$> (L.head sr)
        for_ sr $ \plotFields -> do
          div ! className "splom subplot" $ do
            let plotQ = paretoPlot dsi (fst plotFields) (snd plotFields)
            DF.runQuery plotQ nbrGraph
  where 
  nbrGraph = DF.runQuery (nbrs dsi.paretoRadius) dsi.paretoPoints

paretoPlot :: DataInfo -> String -> String 
           -> Query NeighborGraph (HTML Event)
paretoPlot dsi d1 d2 = do
  limits <- graphNodes `DF.chain` limits2d d1 d2
  plotPoints <- graphNodes `DF.chain` scatterplotPoints dsi.selectedPoints d1 d2
  plotPaths <- graphLinks `DF.chain` paretoPlotPaths dsi.paretoRadius dsi.selectedFronts d1 d2
  pure $ div do
    PV.paretoVis (fst limits) (snd limits) plotPoints plotPaths

splomPairs :: forall a. List a -> List (List (Tuple a a))
splomPairs xs = case L.uncons xs of
  Just {head:x,tail:L.Nil} -> L.Nil
  Just {head:x,tail:xs'} -> (map (Tuple x) xs') L.: (splomPairs xs')
  Nothing -> L.Nil

