module App.View.ParetoVis where

import Prelude (pure, unit, ($), (<<<))
import App.Data (PointData, LineData)
import App.Events (Event(HoverParetoFront, HoverParetoPoint))
import DOM.Event.Types as ET
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (reactClass, reactClassWithProps)
import React (ReactClass)
import Data.Function.Uncurried (runFn2)
import Data.Maybe (fromMaybe)
import Data.Nullable as N
import Text.Smolder.Markup (EventHandler(..), EventHandlers, on, (#!))
import Unsafe.Coerce (unsafeCoerce)

foreign import paretoVisComponent :: forall props. ReactClass props

paretoVis :: Number -> Number 
          -> Array PointData -> Array LineData 
          -> HTML Event
paretoVis maxX maxY pts lines = _paretoVis props #! onPointHover HoverParetoPoint
                                                 #! onFrontHover HoverParetoFront
  where 
  props =
    { "data-maxX": maxX
    , "data-maxY": maxY
    , "data-paretopoints": pts
    , "data-paretopaths": lines
    --, "onFrontHover": onFrontHover HoverParetoFront
    --, "onPointHover": onPointHover HoverParetoPoint
    }

_paretoVis :: forall props. props -> HTML Event
_paretoVis props = reactClassWithProps paretoVisComponent "paretovis" props (pure unit)

-- sanitizing event handlers
onFrontHover :: forall ev. (Array LineData -> ev) -> EventHandlers (ET.Event -> ev)
onFrontHover s = on "onFrontHover" saniHandler
  where
  -- FIXME: can we remove the unsafe coerce?
  saniHandler = s <<< fromMaybe [] <<< N.toMaybe <<< unsafeCoerce 

onPointHover :: forall ev. (Array PointData -> ev) -> EventHandlers (ET.Event -> ev)
onPointHover s = on "onPointHover" saniHandler
  where
  -- FIXME: can we remove the unsafe coerce?
  saniHandler = s <<< fromMaybe [] <<< N.toMaybe <<< unsafeCoerce 


