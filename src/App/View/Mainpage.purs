module App.View.Mainpage where

import Prelude hiding (div, max, min)
import Math (sqrt)
import App.Data (FieldNames, formatNum)
import App.Events (Event(DataFileChange, LoadStaticFile, ParetoRadiusChange, AngleThreshChange))
import App.State (State(..), DataInfo, FileLoadError(..))
import App.View.ParetoSlices as PS
import Data.Array as A
import Data.Traversable (for_)
import Pux.DOM.HTML (HTML)
import Pux.DOM.Events (DOMEvent, onChange, onClick)
import Text.Smolder.HTML (div, label, h2, input, span, ul, li, p, a)
import Text.Smolder.HTML.Attributes (className, type', min, max, step, value)
import Text.Smolder.Markup ((!), (#!), text)
import Loadable (Loadable(..))
import Data.DataFrame as DF
import Data.Int (toNumber)

-- TODO: load these from disk somehow
dataFiles :: Array String
dataFiles = ["2d_small.csv", "3d_small.csv", "3sphere_50.csv", "sphere_500.csv", "whiskies.csv"]

view :: State -> HTML Event
view (State st) =
  div do
    div ! className "left-panel" $ do
      uploadPanel ! className "data-upload" 
      viewDataInfo st.dataset ! className "data-info"
    viewSlices st.dataset ! className "right-panel"

viewDataInfo :: forall d. Loadable FileLoadError (DataInfo d) -> HTML Event
viewDataInfo Unloaded = div $ text "Nothing yet!"
viewDataInfo Loading = div $ text "Loading..."
viewDataInfo (Failed errs) = viewFileErrors errs
viewDataInfo (Loaded dsi) =
  div do
    label do
      text "Number of rows: "
      span $ text $ show $ DF.rows dsi.paretoPoints
    paretoRangeSlider dsi.fieldNames dsi.paretoRadius
    angleThreshSlider dsi.cosThetaThresh
    label do
      text "Dimensions"
      ul ! className "dimension-list" $ do
        for_ (A.sort dsi.fieldNames) $ \fn -> do
          li $ text fn

viewSlices :: forall d. Loadable FileLoadError (DataInfo d) -> HTML Event
viewSlices Unloaded = div $ text "Nothing yet!"
viewSlices Loading = div $ text "Loading..."
viewSlices (Failed errs) = div $ pure unit
viewSlices (Loaded dsi) = PS.view dsi

viewFileErrors :: FileLoadError -> HTML Event
viewFileErrors NoFile = div $ text "" -- FIXME: should be empty
viewFileErrors (LoadError err) = 
  div do
    p $ text "cannot load file"
    ul ! className "details" $ do
      li $ text err

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

paretoRangeSlider :: forall d. FieldNames d -> Number -> HTML Event
paretoRangeSlider fns r = 
    rangeSlider "Neighbor radius:" ParetoRadiusChange 0.0 maxDist r
  where
  maxDist = sqrt $ toNumber $ A.length fns

angleThreshSlider :: Number -> HTML Event
angleThreshSlider theta = 
  rangeSlider "cos theta threshold:" AngleThreshChange 0.8 1.0 theta
  -- #! onChange ParetoRadiusChange

rangeSlider :: String -> (DOMEvent -> Event) -> Number -> Number -> Number -> HTML Event
rangeSlider name changeEvt minv maxv curv =
  label do
    text name
    div ! className "range-slider" $ do
      label $ text (formatNum minv)
      input ! type' "range" 
            ! min (show minv) 
            ! max (show maxv) 
            ! value (show curv) -- FIXME: why doesn't this work!?!?!
            ! step (show $ maxv / 50.0)
            #! onChange changeEvt
      label $ text (formatNum maxv)
      label $ text (formatNum curv)

