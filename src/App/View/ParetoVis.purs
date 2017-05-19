module App.View.ParetoVis where

import Prelude (pure, unit)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (reactClass, reactClassWithProps)
import React (ReactClass)

foreign import paretoVisComponent :: forall props. ReactClass props

type PointData = Array (Array Number)
type LineData = Array (Array (Array Number))

paretoVis :: forall event. Number -> Number 
                        -> PointData -> LineData 
                        -> HTML event
paretoVis maxX maxY pts lines = _paretoVis 
  { "data-maxX": maxX
  , "data-maxY": maxY
  , "data-paretopoints": pts
  , "data-paretopaths": lines
  }

_paretoVis :: forall event props. props -> HTML event
_paretoVis props = reactClassWithProps paretoVisComponent "paretovis" props (pure unit)

