module App.View.Mainpage where

import Prelude hiding (div, max, min)
import App.Events (Event(DataFileChange))
import App.State (State(..), DataInfo, FileLoadError(..))
import App.View.ParetoSlices as PS
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Pux.DOM.HTML (HTML)
import Pux.DOM.Events (onChange)
import Text.Smolder.HTML (div, label, ul, li, p, select, option)
import Text.Smolder.HTML.Attributes (className, value)
import Text.Smolder.Markup ((!), (#!), text)
import Loadable (Loadable(..))

datasets :: Array (Tuple String String)
datasets = 
  [ Tuple "sphere" "sphere_3d.json"
  , Tuple "5D sphere" "sphere_5d.json"
  , Tuple "Klein bottle" "klein.json"
  , Tuple "Cube" "cube.json"
  , Tuple "Hypercube" "4d_cube.json"
  , Tuple "bernstein_1f-1_3.json" "bernstein_1f-1_3.json"
  , Tuple "bernstein_1f1_3.json" "bernstein_1f1_3.json"
  , Tuple "bernstein_2f-1_3.json" "bernstein_2f-1_3.json"
  , Tuple "bernstein_2f1_3.json" "bernstein_2f1_3.json"
  , Tuple "bernstein_3f-1_3.json" "bernstein_3f-1_3.json"
  , Tuple "bernstein_3f1_3.json" "bernstein_3f1_3.json"
  , Tuple "bernstein_4f-1_3.json" "bernstein_4f-1_3.json"
  , Tuple "bernstein_4f1_3.json" "bernstein_4f1_3.json"
  , Tuple "pos_poly_1f-1_3.json" "pos_poly_1f-1_3.json"
  , Tuple "pos_poly_1f1_3.json" "pos_poly_1f1_3.json"
  , Tuple "pos_poly_2f-1_3.json" "pos_poly_2f-1_3.json"
  , Tuple "pos_poly_2f1_3.json" "pos_poly_2f1_3.json"
  , Tuple "pos_poly_3f-1_3.json" "pos_poly_3f-1_3.json"
  , Tuple "pos_poly_3f1_3.json" "pos_poly_3f1_3.json"
  , Tuple "pos_poly_4f-1_3.json" "pos_poly_4f-1_3.json"
  , Tuple "pos_poly_4f1_3.json" "pos_poly_4f1_3.json"
  , Tuple "difference_1f-1_3.json" "difference_1f-1_3.json"
  , Tuple "difference_1f1_3.json" "difference_1f1_3.json"
  , Tuple "difference_2f-1_3.json" "difference_2f-1_3.json"
  , Tuple "difference_2f1_3.json" "difference_2f1_3.json"
  , Tuple "difference_3f-1_3.json" "difference_3f-1_3.json"
  , Tuple "difference_3f1_3.json" "difference_3f1_3.json"
  , Tuple "difference_4f-1_3.json" "difference_4f-1_3.json"
  , Tuple "difference_4f1_3.json" "difference_4f1_3.json"
  , Tuple "bernstein_1f-1_4.json" "bernstein_1f-1_4.json"
  , Tuple "bernstein_1f1_4.json" "bernstein_1f1_4.json"
  , Tuple "bernstein_2f-1_4.json" "bernstein_2f-1_4.json"
  , Tuple "bernstein_2f1_4.json" "bernstein_2f1_4.json"
  , Tuple "bernstein_3f-1_4.json" "bernstein_3f-1_4.json"
  , Tuple "bernstein_3f1_4.json" "bernstein_3f1_4.json"
  , Tuple "bernstein_4f-1_4.json" "bernstein_4f-1_4.json"
  , Tuple "bernstein_4f1_4.json" "bernstein_4f1_4.json"
  , Tuple "bernstein_5f-1_4.json" "bernstein_5f-1_4.json"
  , Tuple "bernstein_5f1_4.json" "bernstein_5f1_4.json"
  , Tuple "pos_poly_1f-1_4.json" "pos_poly_1f-1_4.json"
  , Tuple "pos_poly_1f1_4.json" "pos_poly_1f1_4.json"
  , Tuple "pos_poly_2f-1_4.json" "pos_poly_2f-1_4.json"
  , Tuple "pos_poly_2f1_4.json" "pos_poly_2f1_4.json"
  , Tuple "pos_poly_3f-1_4.json" "pos_poly_3f-1_4.json"
  , Tuple "pos_poly_3f1_4.json" "pos_poly_3f1_4.json"
  , Tuple "pos_poly_4f-1_4.json" "pos_poly_4f-1_4.json"
  , Tuple "pos_poly_4f1_4.json" "pos_poly_4f1_4.json"
  , Tuple "pos_poly_5f-1_4.json" "pos_poly_5f-1_4.json"
  , Tuple "pos_poly_5f1_4.json" "pos_poly_5f1_4.json"
  , Tuple "difference_1f-1_4.json" "difference_1f-1_4.json"
  , Tuple "difference_1f1_4.json" "difference_1f1_4.json"
  , Tuple "difference_2f-1_4.json" "difference_2f-1_4.json"
  , Tuple "difference_2f1_4.json" "difference_2f1_4.json"
  , Tuple "difference_3f-1_4.json" "difference_3f-1_4.json"
  , Tuple "difference_3f1_4.json" "difference_3f1_4.json"
  , Tuple "difference_4f-1_4.json" "difference_4f-1_4.json"
  , Tuple "difference_4f1_4.json" "difference_4f1_4.json"
  , Tuple "difference_5f-1_4.json" "difference_5f-1_4.json"
  , Tuple "difference_5f1_4.json" "difference_5f1_4.json"
  , Tuple "bernstein_1f-1_5.json" "bernstein_1f-1_5.json"
  , Tuple "bernstein_1f1_5.json" "bernstein_1f1_5.json"
  , Tuple "bernstein_2f-1_5.json" "bernstein_2f-1_5.json"
  , Tuple "bernstein_2f1_5.json" "bernstein_2f1_5.json"
  , Tuple "bernstein_3f-1_5.json" "bernstein_3f-1_5.json"
  , Tuple "bernstein_3f1_5.json" "bernstein_3f1_5.json"
  , Tuple "bernstein_4f-1_5.json" "bernstein_4f-1_5.json"
  , Tuple "bernstein_4f1_5.json" "bernstein_4f1_5.json"
  , Tuple "bernstein_5f-1_5.json" "bernstein_5f-1_5.json"
  , Tuple "bernstein_5f1_5.json" "bernstein_5f1_5.json"
  , Tuple "bernstein_6f-1_5.json" "bernstein_6f-1_5.json"
  , Tuple "bernstein_6f1_5.json" "bernstein_6f1_5.json"
  , Tuple "pos_poly_1f-1_5.json" "pos_poly_1f-1_5.json"
  , Tuple "pos_poly_1f1_5.json" "pos_poly_1f1_5.json"
  , Tuple "pos_poly_2f-1_5.json" "pos_poly_2f-1_5.json"
  , Tuple "pos_poly_2f1_5.json" "pos_poly_2f1_5.json"
  , Tuple "pos_poly_3f-1_5.json" "pos_poly_3f-1_5.json"
  , Tuple "pos_poly_3f1_5.json" "pos_poly_3f1_5.json"
  , Tuple "pos_poly_4f-1_5.json" "pos_poly_4f-1_5.json"
  , Tuple "pos_poly_4f1_5.json" "pos_poly_4f1_5.json"
  , Tuple "pos_poly_5f-1_5.json" "pos_poly_5f-1_5.json"
  , Tuple "pos_poly_5f1_5.json" "pos_poly_5f1_5.json"
  , Tuple "pos_poly_6f-1_5.json" "pos_poly_6f-1_5.json"
  , Tuple "pos_poly_6f1_5.json" "pos_poly_6f1_5.json"
  , Tuple "difference_1f-1_5.json" "difference_1f-1_5.json"
  , Tuple "difference_1f1_5.json" "difference_1f1_5.json"
  , Tuple "difference_2f-1_5.json" "difference_2f-1_5.json"
  , Tuple "difference_2f1_5.json" "difference_2f1_5.json"
  , Tuple "difference_3f-1_5.json" "difference_3f-1_5.json"
  , Tuple "difference_3f1_5.json" "difference_3f1_5.json"
  , Tuple "difference_4f-1_5.json" "difference_4f-1_5.json"
  , Tuple "difference_4f1_5.json" "difference_4f1_5.json"
  , Tuple "difference_5f-1_5.json" "difference_5f-1_5.json"
  , Tuple "difference_5f1_5.json" "difference_5f1_5.json"
  , Tuple "difference_6f-1_5.json" "difference_6f-1_5.json"
  , Tuple "difference_6f1_5.json" "difference_6f1_5.json"
  ]

view :: State -> HTML Event
view (State st) =
  div do
    viewOptions
    viewSlices st.dataset

viewOptions :: HTML Event
viewOptions = div ! className "view-options" $ do
  label $ do
    text "Dataset:"
    select #! onChange DataFileChange $ do
      for_ datasets $ \(Tuple lbl url) ->
        option ! value url $ text lbl

viewSlices :: forall d. Loadable FileLoadError (DataInfo d) -> HTML Event
viewSlices Unloaded = div $ text "Nothing yet!"
viewSlices Loading = div $ text "Loading..."
viewSlices (Failed errs) = viewFileErrors errs
viewSlices (Loaded dsi) = PS.view dsi

viewFileErrors :: FileLoadError -> HTML Event
viewFileErrors NoFile = div $ text "" -- FIXME: should be empty
viewFileErrors (LoadError err) = 
  div do
    p $ text "cannot load file"
    ul ! className "details" $ do
      li $ text err

