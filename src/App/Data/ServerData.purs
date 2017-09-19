module App.Data.ServerData where

import Prelude
import Data.StrMap (StrMap)
import Data.Argonaut (class DecodeJson, decodeJson, (.?))

type SDPoint = StrMap Number
type SDPoints = Array SDPoint
newtype SDCurve = SDCurve
  { d1 :: Int
  , d2 :: Int
  , fpid :: Int
  , x1Start :: Number
  , x1End :: Number
  , x2Start :: Number
  , x2End :: Number
  }
type SDCurves = Array SDCurve

newtype ServerData = ServerData
  { points :: SDPoints
  , curves :: SDCurves
  }

instance decodeJsonServerData :: DecodeJson ServerData where
  decodeJson json = do
    obj <- decodeJson json
    pts <- obj .? "points"
    cs <- obj .? "curves"
    pure $ ServerData {points:pts,curves:cs}

instance decodeJsonSDEdge :: DecodeJson SDCurve where
  decodeJson json = do
    obj <- decodeJson json
    x1s <- obj .? "d1.min"
    x2s <- obj .? "d2.min"
    x1e <- obj .? "d1.max"
    x2e <- obj .? "d2.max"
    d1 <- obj .? "d1"
    d2 <- obj .? "d2"
    fpid <- obj .? "fpid"
    pure $ SDCurve { d1: d1
                   , d2: d2
                   , fpid: fpid
                   , x1Start: x1s
                   , x2Start: x2s
                   , x1End: x1e
                   , x2End: x2e
                   }

