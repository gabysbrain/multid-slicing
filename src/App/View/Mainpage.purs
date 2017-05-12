module App.View.Mainpage where

import Prelude hiding (div, max, min)
import Math (sqrt)
import App.Data (AppData, fieldNames, sortedFieldNames)
import App.Events (Event(DataFileChange))
import App.State (State(..), FileLoadError(..))
import App.View.ParetoSlices as PS
import Data.Traversable (for_)
import Pux.DOM.HTML (HTML)
import Pux.DOM.Events (onChange, onSubmit)
import Text.Smolder.HTML (div, label, h2, h3, button, input, span, ul, li, p)
import Text.Smolder.HTML.Attributes (className, type', min, max, step)
import Text.Smolder.Markup ((!), (#!), text)
import Loadable (Loadable(..))
import Data.DataFrame as DF
import Data.Int (toNumber)
import Data.Set as S

view :: State -> HTML Event
view (State st) =
  div do
    div ! className "left-panel" $ do
      uploadPanel ! className "data-upload" 
      viewDataInfo st.dataset ! className "data-info"
    viewSlices st.dataset ! className "right-panel"

viewDataInfo :: Loadable FileLoadError AppData -> HTML Event
viewDataInfo Unloaded = div $ text "Nothing yet!"
viewDataInfo Loading = div $ text "Loading..."
viewDataInfo (Failed errs) = viewFileErrors errs
viewDataInfo (Loaded ds) = 
  div do
    label do
      text "Number of rows"
      span $ text $ show $ DF.rows ds
    paretoRangeSlider ds
    h3 $ text "Dimensions"
    ul ! className "dimension-list" $ do
      for_ (sortedFieldNames ds) $ \fn -> do
        li $ text fn

viewSlices :: Loadable FileLoadError AppData -> HTML Event
viewSlices Unloaded = div $ text "Nothing yet!"
viewSlices Loading = div $ text "Loading..."
viewSlices (Failed errs) = div $ text ""
viewSlices (Loaded ds) = PS.view ds

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
    label do
      text "Data file:"
      input ! type' "file" #! onChange DataFileChange

paretoRangeSlider :: AppData -> HTML Event
paretoRangeSlider ds = 
  label do
    text "Pareto:"
    input ! type' "range" 
          ! min (show 0) 
          ! max (show maxDist) 
          ! step (show $ maxDist / 20.0)
  where
  maxDist = sqrt $ toNumber $ S.size $ fieldNames ds

