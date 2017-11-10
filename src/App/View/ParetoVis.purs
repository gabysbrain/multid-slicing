module App.View.ParetoVis where

import Prelude (pure, unit, ($), (<<<))
import App.Data (CurvePoint)
import App.Events (Event)
import DOM.Event.Types as ET
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (reactClassWithProps)
import React (ReactClass)
import Data.Maybe (Maybe, fromMaybe)
import Data.Nullable as N
import Text.Smolder.Markup (EventHandlers, on, (#!))
import Unsafe.Coerce (unsafeCoerce)

foreign import paretoVisComponent :: forall props. ReactClass props

paretoVis :: Number -> Number
          -> Array CurvePoint
          -> Array Int
          -> HTML Event
paretoVis maxX maxY lines fps = _paretoVis props 
  where
  props =
    { "data-maxX": maxX
    , "data-maxY": maxY
    , "data-selectedfps": fps
    , "data-hullpaths": lines
    }

_paretoVis :: forall props. props -> HTML Event
_paretoVis props = reactClassWithProps paretoVisComponent "paretovis" props (pure unit)

-- sanitizing event handlers
onHullHover :: forall ev. (Array CurvePoint -> ev) -> EventHandlers (ET.Event -> ev)
onHullHover s = on "onHullHover" saniHandler
  where
  -- FIXME: can we remove the unsafe coerce?
  saniHandler = s <<< fromMaybe [] <<< N.toMaybe <<< unsafeCoerce 

-- sanitizing event handlers
onHullClick :: forall ev. (Maybe CurvePoint -> ev) -> EventHandlers (ET.Event -> ev)
onHullClick s = on "onHullClick" saniHandler
  where
  -- FIXME: can we remove the unsafe coerce?
  saniHandler = s <<< N.toMaybe <<< unsafeCoerce 


