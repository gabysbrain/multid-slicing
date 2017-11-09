module App.Events where

import Prelude
import Loadable (Loadable(..))
import Data.Argonaut (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import App.Data (FieldNames, DataPoints, CurvePoint, SliceData, ptsFromServerData)
import App.Data.ServerData (ServerData(..))
import App.Queries (internalizeData)
import App.Routes (Route)
import App.State (CurveInfo, DataInfo, State(..), FileLoadError(..))
import Control.Monad.Aff (Aff(), attempt)
import Control.Monad.Except (Except, except, throwError, withExcept, runExcept)
import Data.DataFrame as DF
import Data.Either (Either(..), either)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import DOM (DOM)
import Network.HTTP.Affjax (AJAX, get)
import Pux (EffModel, noEffects)
import Pux.DOM.Events (DOMEvent, targetValue)
import Data.Tuple (Tuple(..))

type SD d = Tuple (Tuple (FieldNames d) (DataPoints d)) SliceData

data Event 
  = PageView Route
  | DataFileChange DOMEvent
  | ReceiveData (Except FileLoadError (SD Int)) -- FIXME: should be d
  | HoverSlice (Array CurvePoint)
  | ClickSlice Int Int (Maybe CurvePoint)

type AppEffects fx = (ajax :: AJAX, dom :: DOM | fx)

foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (PageView route) (State st) = noEffects $ State st { route = route, loaded = true }
foldp (ReceiveData d) (State st) = noEffects $ 
  State st { dataset = either Failed (Loaded <<< newDatasetState) $ runExcept d }

-- load the data from the server
foldp (DataFileChange ev) (State st) = 
  { state: State (st { dataset = Loading })
  , effects: [ do
      let fname = targetValue ev
      raw <- loadDataFile fname
      let rawServerData = convErr (raw >>= decodeServerData)
      let sd = rawServerData >>= initServerData

      pure $ Just $ ReceiveData sd
    ]
  }
foldp (HoverSlice slices) (State st@{dataset:Loaded dsi}) = noEffects $
  State st {dataset=Loaded dsi {selectedFocusPoints=foldMap (\g -> Set.singleton g.focusPointId) slices}}
foldp (HoverSlice _) st = noEffects st -- shouldn't work unless data loaded
foldp (ClickSlice d1 d2 slice) (State st@{dataset:Loaded dsi}) = noEffects $ 
  State st {dataset=Loaded dsi {selectedCurve=map (mapClickSlice d1 d2) slice}}
foldp (ClickSlice _ _ _) st = noEffects st -- shouldn't work unless data loaded

newDatasetState :: forall d. SD d -> DataInfo d
newDatasetState (Tuple (Tuple fns pts) curves) =
  { dataPoints: pts
  , fieldNames: fns
  , curves: curves
  , selectedFocusPoints: Set.empty
  , selectedCurve: Nothing
  }

mapClickSlice :: Int -> Int -> CurvePoint -> CurveInfo
mapClickSlice d1 d2 cp = 
  {d1: d1, d2: d2, fpId: cp.focusPointId}

mapErr :: Either String String -> Except FileLoadError String
mapErr = withExcept LoadError <<< except

loadDataFile :: forall e. String -> Aff (ajax::AJAX | e) (Either String String)
loadDataFile fn = do
  let url = "http://localhost:8000/" <> fn
  res <- attempt $ get url
  pure $ case res of
    Left err -> Left $ show err
    Right r  -> Right r.response

initServerData :: forall d
                . ServerData 
               -> Except FileLoadError (SD d)
initServerData (ServerData sd) = do
  -- inside Except
  namesNPoints <- withExcept LoadError $ ptsFromServerData sd.points
  let curves = DF.runQuery internalizeData (DF.init sd.curves)
  pure $ Tuple namesNPoints curves

dsd :: String -> Except FileLoadError ServerData
dsd = decodeServerData >>> convErr

decodeServerData :: String -> Either String ServerData
decodeServerData raw = do
  json <- jsonParser raw
  decodeJson json
  --where decode r = decodeJson r :: Either String ServerData

convErr :: forall a. Either String a -> Except FileLoadError a
convErr = either (throwError <<< LoadError) pure

