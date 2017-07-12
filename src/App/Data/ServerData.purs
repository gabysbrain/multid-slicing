module App.Data.ServerData where

import Prelude
import Data.StrMap (StrMap)
import Data.Argonaut (class DecodeJson, decodeJson, (.?))

type SDPoint = StrMap Number
type SDPoints = Array SDPoint
newtype SDEdge = SDEdge {p1::Int, p2::Int}
type SDEdges = Array SDEdge

newtype ServerData = ServerData
  { paretoPoints :: SDPoints
  , simplexEdges :: SDEdges
  }

instance decodeJsonServerData :: DecodeJson ServerData where
  decodeJson json = do
    obj <- decodeJson json
    pts <- obj .? "paretoPoints"
    edgs <- obj .? "simplexEdges"
    pure $ ServerData {paretoPoints:pts,simplexEdges:edgs}

instance decodeJsonSDEdge :: DecodeJson SDEdge where
  decodeJson json = do
    obj <- decodeJson json
    p1 <- obj .? "p1"
    p2 <- obj .? "p2"
    pure $ SDEdge {p1:p1,p2:p2}

