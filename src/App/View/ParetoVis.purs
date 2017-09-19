module App.View.ParetoVis where

import Prelude (pure, unit, ($), (<<<))
import App.Data (CurvePoint)
import App.Events (Event(HoverSlice))
import DOM.Event.Types as ET
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (reactClassWithProps)
import React (ReactClass)
import Data.Maybe (fromMaybe)
import Data.Nullable as N
import Text.Smolder.Markup (EventHandlers, on, (#!))
import Unsafe.Coerce (unsafeCoerce)

foreign import paretoVisComponent :: forall props. ReactClass props

paretoVis :: Number -> Number
          -> Array CurvePoint
          -> HTML Event
paretoVis maxX maxY lines = _paretoVis props 
                                  -- #! onPointHover HoverParetoPoint
                                  -- #! onFrontHover HoverParetoFront
  where 
  props =
    { "data-maxX": maxX
    , "data-maxY": maxY
    --, "data-paretopoints": pts
    , "data-hullpaths": lines
    , "onHullHover": onHullHover HoverSlice
    --, "onPointHover": onPointHover HoverParetoPoint
    }

_paretoVis :: forall props. props -> HTML Event
_paretoVis props = reactClassWithProps paretoVisComponent "paretovis" props (pure unit)

-- sanitizing event handlers
onHullHover :: forall ev. (Array CurvePoint -> ev) -> EventHandlers (ET.Event -> ev)
onHullHover s = on "onHullHover" saniHandler
  where
  -- FIXME: can we remove the unsafe coerce?
  saniHandler = s <<< fromMaybe [] <<< N.toMaybe <<< unsafeCoerce 

{--onPointHover :: forall ev. (Array PointData2D -> ev) -> EventHandlers (ET.Event -> ev)--}
{--onPointHover s = on "onPointHover" saniHandler--}
  {--where--}
  {---- FIXME: can we remove the unsafe coerce?--}
  {--saniHandler = s <<< fromMaybe [] <<< N.toMaybe <<< unsafeCoerce --}


