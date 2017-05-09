module App.View.Mainpage where

import Prelude hiding (div)
import App.Data (AppData, fieldNames)
import App.Events (Event(DataFileChange))
import App.State (State(..), FileLoadError(..))
import App.View.ParetoSlices as PS
import Data.Traversable (for_)
import Pux.DOM.HTML (HTML)
import Pux.DOM.Events (onChange, onSubmit)
import Text.Smolder.HTML (div, label, h2, h3, button, input, span, ul, li, p)
import Text.Smolder.HTML.Attributes (className, type')
import Text.Smolder.Markup ((!), (#!), text)
import Loadable (Loadable(..))
import Data.DataFrame as DF

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
    label do
      text "Pareto:"
      input ! type' "range"
    h3 $ text "Dimensions"
    ul ! className "dimension-list" $ do
      li $ text "dim1" -- FIXME

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

uploadPanel = 
  div do
    label do
      text "Data file:"
      input ! type' "file" #! onChange DataFileChange
    

