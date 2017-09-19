module App.View.ParetoSlices where

import Prelude hiding (div)
import App.Data (FieldNames, SliceData)
import App.Events (Event)
import App.State (DataInfo)
import App.Queries (curve2dFilter)
import App.View.ParetoVis as PV
import Data.Array as A
import Data.DataFrame as DF
import Data.DataFrame (Query)
import Data.Foldable (foldMap)
import Data.List (List)
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
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
            let plotQ = paretoPlot d1 d2 dsi.selectedFocusPoints
            DF.runQuery plotQ dsi.curves
  where 
  sortedNames = L.sort $ fldIdxs dsi.fieldNames

paretoPlot :: Int -> Int -> Set Int -> Query SliceData (HTML Event)
paretoPlot d1 d2 fps = do
  curves2d <- curve2dFilter d1 d2
  pure $ div do
    PV.paretoVis 1.0 1.0 curves2d (stoa fps)

fldIdxs :: forall d. FieldNames d -> List (Tuple Int String)
fldIdxs fns = L.zip (L.range 0 (A.length fns)) (L.fromFoldable fns)

splomPairs :: forall a. List a -> List (List (Tuple a a))
splomPairs xs = L.transpose $ map L.reverse $ splomPairs' xs

splomPairs' :: forall a. List a -> List (List (Tuple a a))
splomPairs' xs = case L.uncons xs of
  Just {head:x,tail:L.Nil} -> L.Nil
  Just {head:x,tail:xs'} -> (map (Tuple x) xs') L.: (splomPairs xs')
  Nothing -> L.Nil

stoa :: forall a. Set a -> Array a
stoa = foldMap pure

