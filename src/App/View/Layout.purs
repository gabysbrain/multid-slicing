module App.View.Layout where

import App.View.Mainpage as Mainpage
import App.View.NotFound as NotFound
import App.Routes (Route(NotFound, Home))
import App.State (State(..))
import App.Events (Event)
import Color (rgb)
import Control.Bind (discard)
import Data.Function (($), (#))
import Pux.DOM.HTML (HTML, style)
import Text.Smolder.HTML (div)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup ((!))

view :: State -> HTML Event
view (State st) =
  div ! className "app" $ do
    case st.route of
      (Home) -> Mainpage.view (State st)
      (NotFound url) -> NotFound.view (State st)

