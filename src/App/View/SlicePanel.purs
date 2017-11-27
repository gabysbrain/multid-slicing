module App.View.SlicePanel where

import Prelude (pure, unit, ($), (<<<))
import App.Data (CurvePoint, Point2D)
import App.Events (Event)
import DOM.Event.Types as ET
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (reactClassWithProps)
import React (ReactClass)
import Data.Maybe (Maybe, fromMaybe)
import Data.Nullable as N
import Text.Smolder.Markup (EventHandlers, on)
import Unsafe.Coerce (unsafeCoerce)

foreign import slicePanelComponent :: forall props. ReactClass props

globalSlicePanel :: Number -> Number
          -> Array CurvePoint
          -> Array Int
          -> HTML Event
globalSlicePanel maxX maxY lines fps = _slicePanel props 
  where
  props =
    { "data-maxX": maxX
    , "data-maxY": maxY
    , "data-selectedfps": fps
    , "data-hullpaths": lines
    }

localSlicePanel :: Number -> Number
          -> Array CurvePoint
          -> Array CurvePoint
          -> Array Point2D
          -> Array Point2D
          -> HTML Event
localSlicePanel maxX maxY lines lines2 targetFps fps = _slicePanel props 
  where
  props =
    { "data-maxX": maxX
    , "data-maxY": maxY
    , "data-hullpaths": lines
    , "data-fppaths": lines2
    , "data-fp-targets": targetFps
    , "data-focuspoints": fps
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

onFPDrag :: forall ev. (Maybe Point2D -> ev) -> EventHandlers (ET.Event -> ev)
onFPDrag s = on "onPointDrag" saniHandler
  where
  -- FIXME: can we remove the unsafe coerce?
  saniHandler = s <<< N.toMaybe <<< unsafeCoerce 

onFPRelease :: forall ev. (Maybe Point2D -> ev) -> EventHandlers (ET.Event -> ev)
onFPRelease s = on "onPointRelease" saniHandler
  where
  -- FIXME: can we remove the unsafe coerce?
  saniHandler = s <<< N.toMaybe <<< unsafeCoerce 

