module App.View.Mainpage where

import Prelude hiding (div, max, min)
import Math (sqrt)
import App.Data (AppData, fieldNames, sortedFieldNames, formatNum)
import App.Events (Event(DataFileChange, LoadStaticFile, ParetoRadiusChange, AngleThreshChange))
import App.State (State(..), DataInfo, FileLoadError(..))
import App.View.ParetoSlices as PS
import Data.Set (Set)
import Data.Traversable (for_)
import Pux.DOM.HTML (HTML)
import Pux.DOM.Events (DOMEvent, onChange, onSubmit, onClick)
import Text.Smolder.HTML (div, label, h2, h3, button, input, span, ul, li, p, a)
import Text.Smolder.HTML.Attributes (className, type', min, max, step, value)
import Text.Smolder.Markup ((!), (#!), text)
import Loadable (Loadable(..))
import Data.DataFrame as DF
import Data.Int (toNumber)
import Data.Set as S

-- TODO: load these from disk somehow
dataFiles :: Array String
dataFiles = ["2d_small.csv", "3d_small.csv", "sphere_50.csv", "whiskies.csv"]

view :: State -> HTML Event
view (State st) =
  div do
    div ! className "left-panel" $ do
      uploadPanel ! className "data-upload" 
      viewDataInfo st.dataset ! className "data-info"
    viewSlices st.dataset ! className "right-panel"

viewDataInfo :: Loadable FileLoadError DataInfo -> HTML Event
viewDataInfo Unloaded = div $ text "Nothing yet!"
viewDataInfo Loading = div $ text "Loading..."
viewDataInfo (Failed errs) = viewFileErrors errs
viewDataInfo (Loaded dsi) =
  div do
    label do
      text "Number of rows: "
      span $ text $ show $ DF.rows dsi.paretoPoints
    paretoRangeSlider dsi.paretoPoints dsi.paretoRadius
    angleThreshSlider dsi.cosThetaThresh
    label do
      text "Dimensions"
      ul ! className "dimension-list" $ do
        for_ (sortedFieldNames dsi.paretoPoints) $ \fn -> do
          li $ text fn

viewSlices :: Loadable FileLoadError DataInfo -> HTML Event
viewSlices Unloaded = div $ text "Nothing yet!"
viewSlices Loading = div $ text "Loading..."
viewSlices (Failed errs) = div $ pure unit
viewSlices (Loaded dsi) = PS.view dsi

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
    rangeSlider "Pareto radius:" ParetoRadiusChange 0.0 maxDist r
  where
  maxDist = sqrt $ toNumber $ S.size $ fieldNames ds

angleThreshSlider :: Number -> HTML Event
angleThreshSlider theta = 
  rangeSlider "cos theta threshold:" AngleThreshChange 0.0 1.0 theta
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
            ! step (show $ maxv / 20.0)
            #! onChange changeEvt
      label $ text (formatNum maxv)
      label $ text (formatNum curv)

