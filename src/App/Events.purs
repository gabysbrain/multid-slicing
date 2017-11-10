module App.Events where

import Prelude
import Loadable (Loadable(..))
import Data.Argonaut (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import App.Data (FieldNames, DataPoints, CurvePoint, SliceData, ptsFromServerData)
import App.Data.ServerData (ServerData(..))
import App.Queries (internalizeData)
import App.Routes (Route)
import App.State (CurveInfo, DataInfo, SelectState(..), 
                  State(..), FileLoadError(..))
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

-- user event handling
foldp (HoverSlice slices) (State st@{dataset:Loaded dsi}) = noEffects $
  State st {dataset=Loaded dsi {selectState=foldHoverSlice slices dsi.selectState}}
foldp (HoverSlice _) st = noEffects st -- shouldn't work unless data loaded
foldp (ClickSlice d1 d2 cs) (State st@{dataset:Loaded dsi}) = noEffects $ 
  State st {dataset=Loaded dsi {selectState=foldClickSlice d1 d2 cs dsi.selectState}}
foldp (ClickSlice _ _ _) st = noEffects st -- shouldn't work unless data loaded

newDatasetState :: forall d. SD d -> DataInfo d
newDatasetState (Tuple (Tuple fns pts) curves) =
  { dataPoints: pts
  , fieldNames: fns
  , curves: curves
  , selectState: Global {selectedFocusPoints: Set.empty}
  }

foldHoverSlice :: Array CurvePoint -> SelectState -> SelectState
foldHoverSlice slices (Global st) = Global st 
  { selectedFocusPoints=foldMap (\g -> Set.singleton g.focusPointId) slices }
foldHoverSlice _ st@(Local _) = st

foldClickSlice :: Int -> Int -> Maybe CurvePoint -> SelectState -> SelectState
foldClickSlice _ _ Nothing (Local st) = 
  Global { selectedFocusPoints: Set.empty }
foldClickSlice _ _ Nothing st@(Global _) = st
foldClickSlice d1 d2 (Just cp) _ =
  Local { selectedCurve: {d1: d1, d2: d2, fpId: cp.focusPointId} }

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

