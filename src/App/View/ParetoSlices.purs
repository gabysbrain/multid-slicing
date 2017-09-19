module App.View.ParetoSlices where

import Prelude hiding (div)
import App.Data (FieldNames, CurveData)
import App.Events (Event)
import App.State (DataInfo)
import App.Queries (scatterplotPoints, paretoPlotPaths)
import App.View.ParetoVis as PV
import Data.Array as A
import Data.DataFrame as DF
import Data.DataFrame (Query)
import Data.List (List)
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Traversable (for_)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (div, label)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup ((!), text)

view :: forall d. DataInfo d -> HTML Event
view dsi = div $ 
  div ! className "splom-view" $ do
    div ! className "splom dims x-axis" $ do
      -- labels for x-axes
      div $ pure unit -- empty cell to offset the axis labels
      for_ sortedNames $ \(Tuple _ fn) -> do
        label ! className "dim-label" $ text fn
    for_ (splomPairs sortedNames) $ \row -> do
      let yField = fromMaybe "" $ snd <$> snd <$> L.head row
      div ! className "splom row" $ do
        --div ! className "splom dims y-axis" $ 
        label ! className "dim-label" $ text yField
        for_ row $ \plotFields -> do
          let d1 = fst $ fst plotFields
              d2 = fst $ snd plotFields
          div ! className "splom subplot" $ do
            let plotQ = paretoPlot dsi d1 d2
            DF.runQuery plotQ dsi.neighborGraph
  where 
  sortedNames = L.sort $ fldIdxs dsi.fieldNames

paretoPlot :: forall d
            . DataInfo d -> Int -> Int 
           -> Query CurveData (HTML Event)
paretoPlot dsi d1 d2 = do
  limits <- graphNodes `DF.chain` limits2d d1 d2
  plotPoints <- graphNodes `DF.chain` scatterplotPoints dsi.selectedPoints d1 d2
  plotPaths <- graphLinks 
    `DF.chain` paretoPlotPaths dsi.paretoRadius dsi.selectedFronts d1 d2
  let pps' = A.filter (\l -> l.cosTheta >= dsi.cosThetaThresh) plotPaths
  pure $ div do
    PV.paretoVis (fst limits) (snd limits) plotPoints plotPaths dsi.cosThetaThresh

fldIdxs :: forall d. FieldNames d -> List (Tuple Int String)
fldIdxs fns = L.zip (L.range 0 (A.length fns)) (L.fromFoldable fns)

splomPairs :: forall a. List a -> List (List (Tuple a a))
splomPairs xs = L.transpose $ map L.reverse $ splomPairs' xs

splomPairs' :: forall a. List a -> List (List (Tuple a a))
splomPairs' xs = case L.uncons xs of
  Just {head:x,tail:L.Nil} -> L.Nil
  Just {head:x,tail:xs'} -> (map (Tuple x) xs') L.: (splomPairs xs')
  Nothing -> L.Nil

