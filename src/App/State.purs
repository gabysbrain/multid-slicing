module App.State where

import App.Config (config)
import App.Data (AppData, CsvError)
import App.Routes (Route, match, toURL)
import Control.Applicative (pure)
import Control.Bind (bind)
import Data.Array as A
import Data.Foreign (MultipleErrors)
import Data.Function (($))
import Data.List.Types (NonEmptyList)
import Data.Set (Set)
import Data.Set as Set
import Loadable (Loadable(..))

data FileLoadError
  = NoFile
  | LoadError MultipleErrors
  | ParseError (NonEmptyList CsvError)

type DataInfo =
  { paretoPoints :: AppData
  , selectedPoints :: Set Int
  , selectedFronts :: Set Int
  , paretoRadius :: Number
  , cosThetaThresh ::Number
  }

data State = State
  { title :: String
  , route :: Route
  , loaded :: Boolean
  , dataset :: Loadable FileLoadError DataInfo
  }

init :: String -> State
init url = State
  { title: config.title
  , route: match url
  , loaded: false
  , dataset: Unloaded
  }
