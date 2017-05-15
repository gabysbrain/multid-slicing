module App.State where

import App.Config (config)
import App.Data (AppData, CsvError)
import App.Routes (Route, match, toURL)
import Control.Applicative (pure)
import Control.Bind (bind)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Foreign (F, ForeignError, MultipleErrors, readString)
import Data.Function (($))
import Data.List.Types (NonEmptyList)
import Loadable (Loadable(..))

data FileLoadError
  = NoFile
  | LoadError MultipleErrors
  | ParseError (NonEmptyList CsvError)

data State = State
  { title :: String
  , route :: Route
  , loaded :: Boolean
  , dataset :: Loadable FileLoadError AppData
  , paretoRadius :: Number
  }

instance decodeJsonState :: DecodeJson State where
  decodeJson json = do
    obj <- decodeJson json
    title <- obj .? "title"
    url <- obj .? "route"
    loaded <- obj .? "loaded"
    r <- obj .? "paretoRadius"
    pure $ State
      { title
      , loaded
      , route: match url
      , dataset: Unloaded -- FIXME: need to serialize the data frame
      , paretoRadius: r
      }

instance encodeJsonState :: EncodeJson State where
  encodeJson (State st) =
       "title" := st.title
    ~> "route" := toURL st.route
    ~> "loaded" := st.loaded
    ~> "paretoRadius" := st.paretoRadius
    ~> jsonEmptyObject

init :: String -> State
init url = State
  { title: config.title
  , route: match url
  , loaded: false
  , dataset: Unloaded
  , paretoRadius: 1.0
  }
