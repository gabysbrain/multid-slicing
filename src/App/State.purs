module App.State where

import App.Config (config)
import App.Data (ParetoPoints, NeighborGraph, FieldNames)
import App.Routes (Route, match)
import Data.Set (Set)
import Loadable (Loadable(..))

data FileLoadError
  = NoFile
  | LoadError String

type DataInfo d =
  { paretoPoints :: ParetoPoints d
  , fieldNames :: FieldNames d
  , selectedPoints :: Set Int
  , selectedFronts :: Set Int
  , paretoRadius :: Number
  , cosThetaThresh ::Number
  , neighborGraph :: NeighborGraph d
  }

data State = State
  { title :: String
  , route :: Route
  , loaded :: Boolean
  , dataset :: Loadable FileLoadError (DataInfo Int) -- FIXME: this is wrong. it should be the size
  }

init :: String -> State
init url = State
  { title: config.title
  , route: match url
  , loaded: false
  , dataset: Unloaded
  }
