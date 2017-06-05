module Util where

import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, empty, fold, insert, lookup)

joinWith :: forall v1 v2 v3. (v1 -> v2 -> v3) -> StrMap v1 -> StrMap v2 -> StrMap v3
joinWith f m1 m2 = fold go empty m1
  where
  go m k v = case lookup k m2 of
    Just v2 -> insert k (f v v2) m
    Nothing -> m

