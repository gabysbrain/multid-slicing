module App.State where

import App.Config (config)
import App.Data (SliceData, DataPoints, FieldNames)
import App.Routes (Route, match)
import Data.Maybe (Maybe)
import Data.Set (Set)
import Loadable (Loadable(..))

type CurveInfo = 
  { d1 :: Int
  , d2 :: Int
  , fpId :: Int
  }

data FileLoadError
  = NoFile
  | LoadError String

type DataInfo d =
  { fieldNames :: FieldNames d
  , dataPoints :: DataPoints d
  , curves :: SliceData
  , selectedFocusPoints :: Set Int
  , selectedCurve :: Maybe CurveInfo
  }

data State = State
  { title :: String
  , route :: Route
  , loaded :: Boolean
  , dataset :: Loadable FileLoadError (DataInfo Int) -- FIXME: wrong type
  }

init :: String -> State
init url = State
  { title: config.title
  , route: match url
  , loaded: false
  , dataset: Unloaded
  }
