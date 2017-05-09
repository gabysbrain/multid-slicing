module App.View.ParetoSlices where

import Prelude hiding (div)
import App.Data (AppData)
import App.Events (Event)
import Data.DataFrame as DF
import Pareto (paretoSet)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (div, label, h2, h3, button, input, span, ul, li, p)
import Text.Smolder.HTML.Attributes (className, type')
import Text.Smolder.Markup ((!), (#!), text)

view :: AppData -> HTML Event
view pts = 
  div $ text ("test: " <> show (DF.rows ppts))
  where
  ppts = DF.runQuery paretoSet pts

