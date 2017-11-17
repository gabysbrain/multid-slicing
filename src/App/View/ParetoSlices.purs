module App.View.ParetoSlices where

import Prelude hiding (div)
import App.Data (FieldNames, SliceData)
import App.Events (Event(HoverSlice, ClickSlice))
import App.State (DataInfo, SelectState(..))
import App.Queries (curve2dFilter, fpFilter)
import App.View.SlicePanel as SP
import Data.Array as A
import Data.DataFrame as DF
import Data.DataFrame (Query)
import Data.Foldable (foldMap)
import Data.List (List)
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Traversable (for_)
import Pux.DOM.HTML (HTML)
import Pux.DOM.Events (onClick)
import Text.Smolder.HTML (div, label, button)
import Text.Smolder.HTML.Attributes (className, disabled)
import Text.Smolder.Markup ((!), (#!), text)

--import Debug.Trace (traceShow)

view :: forall d. DataInfo d -> HTML Event
view dsi = div $ do
  deselectButton dsi.selectState
  div ! className "splom-view" $ do
    div ! className "splom dims x-axis" $ do
      -- labels for x-axes
      div $ pure unit -- empty cell to offset the axis labels
      for_ (colNames sortedNames) $ \(Tuple _ fn) -> do
        label ! className "dim-label" $ text fn
    --traceShow flds
    for_ (splomPairs sortedNames) $ \row -> do
      let yField = fromMaybe "" $ snd <$> snd <$> L.head row
      div ! className "splom row" $ do
        --div ! className "splom dims y-axis" $ 
        label ! className "dim-label" $ text yField
        for_ row $ \plotFields -> do
          let d1 = fst $ fst plotFields
              d2 = fst $ snd plotFields
          div ! className "splom subplot" $ do
            let plotQ = paretoPlot d1 d2 dsi.selectState
            DF.runQuery plotQ dsi.curves
  where 
  sortedNames = L.sort $ fldIdxs dsi.fieldNames

paretoPlot :: Int -> Int -> SelectState -> Query SliceData (HTML Event)
-- global view shows all slices
paretoPlot d1 d2 (Global st) = do
  curves2d <- curve2dFilter d1 d2
  pure $ div do
    SP.slicePanel 1.0 1.0 curves2d (stoa st.selectedFocusPoints)
      #! SP.onHullHover HoverSlice
      #! SP.onHullClick (ClickSlice d1 d2)
-- local view shows one slice
paretoPlot d1 d2 (Local st) = do
  curves2d <- fpFilter st.selectedCurve.fpId `DF.chain`
              curve2dFilter d1 d2
  pure $ div do
    SP.slicePanel 1.0 1.0 curves2d []
      #! SP.onHullClick (ClickSlice d1 d2)

deselectButton :: SelectState -> HTML Event
deselectButton (Local _) =
  deselectButtonBase #! onClick (const $ ClickSlice 1 1 Nothing)
deselectButton (Global _) =
  deselectButtonBase ! disabled "true"

deselectButtonBase :: HTML Event
deselectButtonBase = button ! className "deselect-button" $ text "deselect"

fldIdxs :: forall d. FieldNames d -> List (Tuple Int String)
fldIdxs fns = L.zip (L.range 0 (A.length fns)) (L.fromFoldable fns)

colNames :: List (Tuple Int String) -> List (Tuple Int String)
colNames = fromMaybe L.Nil <<< L.init 

splomPairs :: forall a. List a -> List (List (Tuple a a))
splomPairs = L.transpose <<< mapRev <<< splomPairs'

splomPairs' :: forall a. List a -> List (List (Tuple a a))
splomPairs' xs = case L.uncons xs of 
  Nothing -> L.Nil
  Just {head:x,tail:L.Nil} -> L.Nil
  Just {head:x,tail:xs'} -> (map (Tuple x) xs') L.: (splomPairs' xs')

stoa :: forall a. Set a -> Array a
stoa = foldMap pure

-- don't know why but map L.reverse isn't working right...
mapRev :: forall a. List (List a) -> List (List a)
mapRev xs = case L.uncons xs of
  Nothing -> L.Nil
  Just uc -> (L.reverse uc.head) L.: (mapRev uc.tail)

