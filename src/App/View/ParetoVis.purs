module App.View.ParetoVis where

import Prelude (pure, unit)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (reactClass, reactClassWithProps)
import React (ReactClass)

foreign import paretoVisComponent :: forall props. ReactClass props

type ParetoData = Array (Array (Array Number))

paretoVis :: forall event. Number -> Number -> ParetoData -> HTML event
paretoVis maxX maxY pts = _paretoVis 
  { "data-maxX": maxX
  , "data-maxY": maxY
  , "data-paretopaths": pts
  }

_paretoVis :: forall event props. props -> HTML event
_paretoVis props = reactClassWithProps paretoVisComponent "paretovis" props (pure unit)

