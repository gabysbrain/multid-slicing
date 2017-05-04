module App.Events where

import Prelude
import Loadable (Loadable(..))
import App.Data (AppData, fromCsv)
import App.Routes (Route)
import App.State (State(..), FileLoadError(..))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Except (Except, throwError, runExcept, withExcept)
import Data.Either (Either(..), either)
import Data.Foreign (readString)
import Data.Maybe (Maybe(..))
import Data.Nullable as Null
import DOM (DOM)
import DOM.Event.Types as EVT
import DOM.File.FileList (item)
import DOM.File.FileReader (fileReader, result, readAsText)
import DOM.File.Types (File, FileList, fileToBlob)
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects)
import Pux.DOM.Events (DOMEvent)

data Event 
  = PageView Route
  | DataFileChange DOMEvent
  -- | RequestData
  | ReceiveData (Except FileLoadError AppData)

foreign import targetFileList :: DOMEvent -> FileList

type AppEffects fx = (ajax :: AJAX, dom :: DOM | fx)

foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (PageView route) (State st) = noEffects $ State st { route = route, loaded = true }
foldp (ReceiveData d) (State st) = noEffects $ 
  State st { dataset = either Failed Loaded $ runExcept d }
-- load the data from the file the user specified
foldp (DataFileChange ev) (State st) = 
  { state: State (st { dataset = Loading })
  , effects: [ do
      let f = userFile ev :: Except FileLoadError File
      --raw <- withExcept FileError $ readFile f
      raw <- liftEff $ readFile' f
      --let ds = (except $ either (Left <<< UnknownError <<< show) id raw) >>= parseCsv
      let ds = raw >>= parseCsv
      pure $ Just $ ReceiveData $ ds
    ]
  }

readFile' :: forall eff. Except FileLoadError File -> Eff (dom :: DOM | eff) (Except FileLoadError String)
readFile' f = case runExcept f of
  Left e -> pure $ throwError e
  Right f' -> readFile f'

readFile :: forall eff. File -> Eff (dom :: DOM | eff) (Except FileLoadError String)
readFile f = do
  fr <- fileReader
  readAsText (fileToBlob f) fr
  res <- result fr
  pure $ withExcept LoadError $ readString res

userFile :: EVT.Event -> Except FileLoadError File
userFile ev = case Null.toMaybe $ item 0 fl of
    Nothing -> throwError NoFile
    Just f -> pure f
  where
  fl = targetFileList ev -- FIXME: replace with readFileList

parseCsv :: String -> (Except FileLoadError AppData)
parseCsv = withExcept ParseError <<< fromCsv

