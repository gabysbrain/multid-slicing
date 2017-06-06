module App.State where

import App.Config (config)
import App.Data (AppData, CsvError)
import App.Routes (Route, match, toURL)
import Control.Applicative (pure)
import Control.Bind (bind)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.?), (:=), (~>))
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

instance decodeJsonState :: DecodeJson State where
  decodeJson json = do
    obj <- decodeJson json
    title <- obj .? "title"
    url <- obj .? "route"
    loaded <- obj .? "loaded"
    pure $ State
      { title
      , loaded
      , route: match url
      , dataset: Unloaded -- FIXME: need to serialize the data frame
      }

instance encodeJsonState :: EncodeJson State where
  encodeJson (State st) =
       "title" := st.title
    ~> "route" := toURL st.route
    ~> "loaded" := st.loaded
    ~> jsonEmptyObject

init :: String -> State
init url = State
  { title: config.title
  , route: match url
  , loaded: false
  , dataset: Unloaded
  }
