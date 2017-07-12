module App.Events where

import Prelude
import Loadable (Loadable(..))
import Data.Argonaut (decodeJson)
import App.Data (ParetoPoints, FieldNames, PointData2D, LineData2D, NeighborGraph, ptsFromServerData, ngFromServerData)
import App.Data.ServerData (ServerData(..))
import App.Queries (nbrs)
import App.Routes (Route)
import App.State (DataInfo, State(..), FileLoadError(..))
import Control.Monad.Aff (Aff(), makeAff, attempt)
import Control.Monad.Eff (Eff())
import Control.Monad.Except (Except, except, throwError, withExcept, runExcept)
import Data.DataFrame as DF
import Data.Either (Either(..), either)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Number as N
import Data.Set as Set
import DOM (DOM)
import DOM.Event.Types as EVT
import DOM.File.FileList (item)
import DOM.File.Types (File, FileList)
import Network.HTTP.Affjax (AJAX, get, post)
import Pux (EffModel, noEffects)
import Pux.DOM.Events (DOMEvent, targetValue)
import Data.Tuple (Tuple(..), snd)
import Data.String (Pattern(..), Replacement(..), replaceAll)

type SD d = Tuple (Tuple (FieldNames d) (ParetoPoints d)) (NeighborGraph d)

data Event 
  = PageView Route
  | ParetoRadiusChange DOMEvent
  | AngleThreshChange DOMEvent
  | LoadStaticFile String DOMEvent
  | DataFileChange DOMEvent
  | ReceiveData (Except FileLoadError (SD Int)) -- FIXME: should be d
  | HoverParetoFront (Array LineData2D)
  | HoverParetoPoint (Array PointData2D)
  -- | StartParetoFilter AppData
  -- | FinishParetoFilter AppData

foreign import targetFileList :: DOMEvent -> FileList
foreign import readFileAsText :: forall e
                               . (String -> Eff e Unit)
                              -> File
                              -> Eff e Unit

type AppEffects fx = (ajax :: AJAX, dom :: DOM | fx)

foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (PageView route) (State st) = noEffects $ State st { route = route, loaded = true }
foldp (ParetoRadiusChange ev) (State st) = noEffects $ 
  case N.fromString (targetValue ev) of
    Just r  -> updateRadius r (State st)
    Nothing -> State st
foldp (AngleThreshChange ev) (State st) = noEffects $ 
  case N.fromString (targetValue ev) of
    Just t  -> updateAngleThresh t (State st)
    Nothing -> State st
foldp (ReceiveData d) (State st) = noEffects $ 
  State st { dataset = either Failed (Loaded <<< newDatasetState) $ runExcept d }
foldp (LoadStaticFile fn _) (State st) =
  { state: State (st { dataset = Loading })
  , effects: [ do
      raw <- loadStaticFile fn
      sd <- initServerData' $ mapErr raw
      pure $ Just $ ReceiveData sd
    ]
  }
-- load the data from the file the user specified
foldp (DataFileChange ev) (State st) = 
  { state: State (st { dataset = Loading })
  , effects: [ do
      let f = userFile ev :: Except FileLoadError File
      raw <- readFile' f
      -- FIXME: maybe do the pareto calculatino in a separate async event
      sd <- initServerData' raw

      pure $ Just $ ReceiveData sd
    ]
  }
foldp (HoverParetoFront pfs) (State st@{dataset:Loaded dsi}) = noEffects $
  State st {dataset=Loaded dsi {selectedFronts=foldMap (\g -> Set.singleton g.slabId) pfs}}
foldp (HoverParetoFront _) st = noEffects st
foldp (HoverParetoPoint pts) (State st@{dataset:Loaded dsi}) = noEffects $
  State st {dataset=Loaded dsi {selectedPoints=foldMap (\p -> Set.singleton p.rowId) pts}}
foldp (HoverParetoPoint _) st = noEffects st

newDatasetState :: forall d. SD d -> DataInfo d
newDatasetState (Tuple (Tuple fns pts) ng) =
  { paretoPoints: pts
  , fieldNames: fns
  , selectedPoints: Set.empty
  , selectedFronts: Set.empty
  , paretoRadius: 1.0
  , cosThetaThresh: 1.0
  , neighborGraph: ng
  }

mapErr :: Either String String -> Except FileLoadError String
mapErr = withExcept LoadError <<< except

loadStaticFile :: forall e. String -> Aff (ajax::AJAX | e) (Either String String)
loadStaticFile fn = do
  let url = "/test_data/" <> fn
  res <- attempt $ get url
  pure $ case res of
    Left err -> Left $ show err
    Right r  -> Right r.response

initServerData' :: forall d e. Except FileLoadError String -> Aff (ajax::AJAX | e) (Except FileLoadError (SD d))
initServerData' raw = case runExcept raw of
  Left err   -> pure $ throwError err
  Right raw' -> withExcept LoadError <$> initServerData raw'

initServerData :: forall d e. String -> Aff (ajax::AJAX | e) (Except String (SD d))
initServerData raw = do
  -- send the raw data to the R server to get pareto 
  -- points and neighbor graph
  pd <- except <$> getParetoData raw
  pure $ do -- inside Except
    pd' <- pd
    let (ServerData sd) = pd'
    namesNPoints <- ptsFromServerData sd.paretoPoints
    ng <- ngFromServerData (snd namesNPoints) sd.simplexEdges
    pure $ Tuple namesNPoints ng

getParetoData :: forall e. String -> Aff (ajax::AJAX | e) (Either String ServerData)
getParetoData raw = do
  let encRaw = formEncodeRaw raw
  res <- attempt $ post "http://127.0.0.1:8080/pareto" encRaw
  let decode r = decodeJson r.response :: Either String ServerData
  pure $ either (Left <<< show) decode res

updateRadius :: Number -> State -> State
updateRadius r (State st) = case st.dataset of
  Loaded dsi -> State $ st {dataset=Loaded (
    dsi { paretoRadius = r
        , neighborGraph = DF.runQuery (nbrs r) dsi.paretoPoints
        })}
  _ -> State st

updateAngleThresh :: Number -> State -> State
updateAngleThresh t (State st) = case st.dataset of
  Loaded dsi -> State $ st {dataset=Loaded (dsi {cosThetaThresh=t})}
  _ -> State st

readFile :: forall eff. File -> Aff eff String
readFile f = makeAff (\error success -> readFileAsText success f)

readFile' :: forall eff. Except FileLoadError File -> Aff eff (Except FileLoadError String)
readFile' f = case runExcept f of
  Left err -> pure $ throwError err
  Right f' -> readFile'' f'

readFile'' :: forall eff. File -> Aff eff (Except FileLoadError String)
readFile'' f = do
  contents <- readFile f
  pure $ pure contents

userFile :: EVT.Event -> Except FileLoadError File
userFile ev = case item 0 fl of
    Nothing -> throwError NoFile
    Just f -> pure f
  where
  fl = targetFileList ev -- FIXME: replace with readFileList

-- replaces all newlines with %0A so R can read it
formEncodeRaw :: String -> String
formEncodeRaw = replaceAll (Pattern "\n") (Replacement "%0A")

