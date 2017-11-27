module App.Events where

import Prelude
import Loadable (Loadable(..))
import Data.Argonaut (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array ((!!))
import App.Data (CurvePoint, SliceData, DataPoints, 
                 LocalCurves, LocalCurve, Point2D, 
                 rowId, rowVal, ptsFromServerData, fpsFromServerData, 
                 closestPoint2d)
import App.Data.ServerData (ServerData(..))
import App.Queries (internalizeData, localFp, fpFilter)
import App.Routes (Route)
import App.State (DataInfo, SelectState(..), State(..), FileLoadError(..))
import Control.Monad.Aff (Aff(), attempt)
import Control.Monad.Except (Except, except, throwError, withExcept, runExcept)
import Data.DataFrame as DF
import Data.Either (Either(..), either)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Set as Set
import DOM (DOM)
import Network.HTTP.Affjax (AJAX, get)
import Pux (EffModel, noEffects)
import Pux.DOM.Events (DOMEvent, targetValue)
import Data.Tuple (fst, snd)
import Data.Geom.Point (Point)
import Data.Geom.Point as Pt

data Event 
  = PageView Route
  | DataFileChange DOMEvent
  | ReceiveData (Except FileLoadError (DataInfo Int)) -- FIXME: should be d
  | HoverSlice (Array CurvePoint)
  | ClickSlice Int Int (Maybe CurvePoint)
  | DragFocusPoint Int Int (Maybe Point2D)
  | UpdateFocusPoints Int Int

type AppEffects fx = (ajax :: AJAX, dom :: DOM | fx)

foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (PageView route) (State st) = noEffects $ State st { route = route, loaded = true }
foldp (ReceiveData d) (State st) = noEffects $ 
  State st { dataset = either Failed Loaded $ runExcept d }

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
  State st {dataset=Loaded dsi {selectState=foldClickSlice dsi.focusPoints d1 d2 cs dsi.selectState}}
foldp (ClickSlice _ _ _) st = noEffects st -- shouldn't work unless data loaded
foldp (DragFocusPoint d1 d2 fp) (State st@{dataset:Loaded dsi}) = noEffects $
  State st {dataset=Loaded dsi {selectState=foldFpDrag d1 d2 fp dsi.selectState}}
foldp (DragFocusPoint _ _ _) st = noEffects st -- shouldn't work unless data loaded
foldp (UpdateFocusPoints d1 d2) (State st@{dataset:Loaded dsi}) = noEffects $
  State st {dataset=Loaded $ foldSelectedFps d1 d2 dsi}
foldp (UpdateFocusPoints _ _) st = noEffects st -- shouldn't work unless data loaded

--foldSelectState :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)

foldHoverSlice :: forall d. Array CurvePoint -> SelectState d -> SelectState d
foldHoverSlice slices (Global st) = Global st 
  { selectedFocusPoints=foldMap (\g -> Set.singleton g.focusPointId) slices }
foldHoverSlice _ st@(Local _) = st

foldClickSlice :: forall d
                . DataPoints d
               -> Int -> Int 
               -> Maybe CurvePoint 
               -> SelectState d 
               -> SelectState d
foldClickSlice _ _ _ Nothing (Local st) = 
  Global { selectedFocusPoints: Set.empty }
foldClickSlice _ _ _ Nothing st@(Global _) = st
foldClickSlice fps d1 d2 (Just cp) _ =
       initLocalView fps d1 d2 cp

foldFpDrag :: forall d
            . Int -> Int
           -> Maybe Point2D
           -> SelectState d
           -> SelectState d
foldFpDrag _ _ Nothing lc = lc
foldFpDrag _ _ _       (Global st) = Global st
foldFpDrag d1 d2 (Just pt) (Local st) =
  Local st { localCurves = mergeFps d1 d2 st.localCurves pt }

foldSelectedFps :: forall d. Int -> Int -> DataInfo d -> DataInfo d
foldSelectedFps _ _ dsi@{selectState:Global _} = dsi
foldSelectedFps d1 d2 dsi@{selectState:Local st@{localCurves:lcs}} = 
  dsi {selectState=Local st {localCurves=DF.runQuery (updateLocalCurves d1 d2 dsi.focusPoints dsi.curves) lcs}}

updateLocalCurves :: forall d
                   . Int -> Int 
                  -> DataPoints d -> SliceData 
                  -> DF.Query (LocalCurves d) (LocalCurves d)
updateLocalCurves d1 d2 fps curves = do
  loaded <- DF.filter (rowVal >>> (\r -> r.curves) >>> isJust)
  -- find any newly changed focus points and load their closest curves
  newLoaded <- DF.filter (rowVal >>> (\r -> r.curves) >>> isNothing) `DF.chain`
               DF.mutate (_procLC d1 d2 fps curves)
  pure $ loaded <> newLoaded

_procLC :: forall d
         . Int -> Int 
        -> DataPoints d -> SliceData -> LocalCurve d -> LocalCurve d
_procLC d1 d2 fps curves lc = case closestPoint2d d1 d2 fps (rowVal lc).fp of
  Nothing -> lc
  Just pt -> map (\lc' -> { fp: rowVal pt
                          , curves: Just $ DF.runQuery (fpFilter (rowId pt)) curves})
                 lc

mergeFps :: forall d. Int -> Int -> LocalCurves d -> Point2D -> LocalCurves d
mergeFps d1 d2 curves pt = DF.runQuery (DF.mutate (mergeFpRow d1 d2 pt)) curves

mergeFpRow :: forall d. Int -> Int -> Point2D -> LocalCurve d -> LocalCurve d
mergeFpRow d1 d2 pt lc =
  if rowId pt == rowId lc 
     then map (\c -> {fp:mergeFp d1 d2 (rowVal pt) c.fp, curves: Nothing}) lc
     else lc

mergeFp :: forall d. Int -> Int -> Array Number -> Point d -> Point d
mergeFp d1 d2 xs pt = fromMaybe pt $ _mergeFp d1 d2 xs pt

_mergeFp :: forall d. Int -> Int -> Array Number -> Point d -> Maybe (Point d)
_mergeFp d1 d2 xs pt = do
  x1 <- xs !! 0
  x2 <- xs !! 1
  pure $ Pt.updateAt d1 x1 $ Pt.updateAt d2 x2 pt

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
               -> Except FileLoadError (DataInfo d)
initServerData (ServerData sd) = do
  -- inside Except
  namesNPoints <- withExcept LoadError $ ptsFromServerData sd.points
  fps <- withExcept LoadError $ fpsFromServerData sd.focusPoints
  let curves = DF.runQuery internalizeData (DF.init sd.curves)
  pure $ { dataPoints: snd namesNPoints
         , fieldNames: fst namesNPoints
         , focusPoints: fps
         , curves: curves
         , selectState: Global {selectedFocusPoints: Set.empty}
         }

initLocalView :: forall d
               . DataPoints d 
              -> Int -> Int 
              -> CurvePoint 
              -> SelectState d
initLocalView fps d1 d2 cp = Local 
  { selectedCurve: {d1: d1, d2: d2, fpId: cp.focusPointId} 
  , localCurves: DF.runQuery (localFp cp.focusPointId) fps 
  }

dsd :: String -> Except FileLoadError ServerData
dsd = decodeServerData >>> convErr

decodeServerData :: String -> Either String ServerData
decodeServerData raw = do
  json <- jsonParser raw
  decodeJson json
  --where decode r = decodeJson r :: Either String ServerData

convErr :: forall a. Either String a -> Except FileLoadError a
convErr = either (throwError <<< LoadError) pure

