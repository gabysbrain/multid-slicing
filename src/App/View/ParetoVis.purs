module App.View.ParetoVis where

import Prelude (pure, unit)
import App.Data (PointData, LineData)
import App.Events (Event(HoverParetoFront, HoverParetoPoint))
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (reactClass, reactClassWithProps)
import React (ReactClass)

foreign import paretoVisComponent :: forall props. ReactClass props

paretoVis :: forall event. Number -> Number 
                        -> Array PointData -> Array LineData 
                        -> HTML event
paretoVis maxX maxY pts lines = _paretoVis 
  { "data-maxX": maxX
  , "data-maxY": maxY
  , "data-paretopoints": pts
  , "data-paretopaths": lines
  , "onFrontHover": HoverParetoFront
  , "onPointHover": HoverParetoPoint
  }

_paretoVis :: forall event props. props -> HTML event
_paretoVis props = reactClassWithProps paretoVisComponent "paretovis" props (pure unit)

