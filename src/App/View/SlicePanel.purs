module App.View.SlicePanel where

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

foreign import slicePanelComponent :: forall props. ReactClass props

slicePanel :: Number -> Number
          -> Array CurvePoint
          -> Array Int
          -> HTML Event
slicePanel maxX maxY lines fps = _slicePanel props 
  where
  props =
    { "data-maxX": maxX
    , "data-maxY": maxY
    , "data-selectedfps": fps
    , "data-hullpaths": lines
    }

_slicePanel :: forall props. props -> HTML Event
_slicePanel props = reactClassWithProps slicePanelComponent "paretovis" props (pure unit)

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


