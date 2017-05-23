module App.View.Mainpage where

import Prelude hiding (div, max, min)
import Math (sqrt)
import App.Data (AppData, fieldNames, sortedFieldNames)
import App.Events (Event(DataFileChange, LoadStaticFile, ParetoRadiusChange))
import App.State (State(..), FileLoadError(..))
import App.View.ParetoSlices as PS
import Data.Set (Set)
import Data.Traversable (for_)
import Pux.DOM.HTML (HTML)
import Pux.DOM.Events (onChange, onSubmit, onClick)
import Text.Smolder.HTML (div, label, h2, h3, button, input, span, ul, li, p, a)
import Text.Smolder.HTML.Attributes (className, type', min, max, step, value)
import Text.Smolder.Markup ((!), (#!), text)
import Loadable (Loadable(..))
import Data.DataFrame as DF
import Data.Int (toNumber)
import Data.Set as S

-- TODO: load these from disk somehow
dataFiles :: Array String
dataFiles = ["2d_small.csv", "3d_small.csv", "whiskies.csv"]

view :: State -> HTML Event
view (State st) =
  div do
    div ! className "left-panel" $ do
      uploadPanel ! className "data-upload" 
      viewDataInfo st.dataset st.paretoRadius ! className "data-info"
    viewSlices st.paretoRadius st.selectedPoints st.selectedFronts st.dataset ! className "right-panel"

viewDataInfo :: Loadable FileLoadError AppData -> Number -> HTML Event
viewDataInfo Unloaded _ = div $ text "Nothing yet!"
viewDataInfo Loading _ = div $ text "Loading..."
viewDataInfo (Failed errs) _ = viewFileErrors errs
viewDataInfo (Loaded ds) r = 
  div do
    label do
      text "Number of rows: "
      span $ text $ show $ DF.rows ds
    paretoRangeSlider ds r
    label do
      text "Dimensions"
      ul ! className "dimension-list" $ do
        for_ (sortedFieldNames ds) $ \fn -> do
          li $ text fn

viewSlices :: Number -> Set Int -> Set Int -> Loadable FileLoadError AppData -> HTML Event
viewSlices _ _  _  Unloaded = div $ text "Nothing yet!"
viewSlices _ _  _  Loading = div $ text "Loading..."
viewSlices _ _  _  (Failed errs) = div $ pure unit
viewSlices r sp sf (Loaded ds) = PS.view r sp sf ds

viewFileErrors :: FileLoadError -> HTML Event
viewFileErrors NoFile = div $ text "" -- FIXME: should be empty
viewFileErrors (LoadError errs) = 
  div do
    p $ text "cannot load file"
    ul ! className "details" $ do
      for_ errs $ \e -> do
        li $ text (show e)
viewFileErrors (ParseError errs) = 
  div do
    p $ text "cannot parse csv file"
    ul ! className "details" $ do
      for_ errs $ \e -> do
        li $ text (show e)

uploadPanel :: HTML Event
uploadPanel = 
  div do
    h2 $ text "Import data"
    label do
      text "Upload data"
      input ! type' "file" #! onChange DataFileChange
    label do
      text "Demo data"
      ul ! className "static-files" $ do
        for_ dataFiles $ \fn -> do
          li $
            a #! onClick (LoadStaticFile fn) $
              text fn

paretoRangeSlider :: AppData -> Number -> HTML Event
paretoRangeSlider ds r = 
  label do
    text "Pareto radius:"
    div ! className "range-slider" $ do
      label $ text "0"
      input ! type' "range" 
            ! min (show 0) 
            ! max (show maxDist) 
            ! value (show r) -- FIXME: why doesn't this work!?!?!
            ! step (show $ maxDist / 20.0)
            #! onChange ParetoRadiusChange
      label $ text (show maxDist)
      label $ text (show r)
  where
  maxDist = sqrt $ toNumber $ S.size $ fieldNames ds

