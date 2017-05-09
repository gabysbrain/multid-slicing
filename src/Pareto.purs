module Pareto where

import Prelude

import App.Data (AppData, AppDatum)
import Control.Monad.Loops (whileM, whileM')
import Data.DataFrame (Query)
import Data.DataFrame as DF
import Data.Array as A
import Data.Foldable (sum, or, foldMap)
import Data.List as L
import Data.Maybe (maybe, fromJust, fromMaybe)
import Data.StrMap as SM
import Data.Traversable (for)
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)

import Debug.Trace

paretoSet :: Query AppData AppData
paretoSet = DF.init <$> _paretoSet

-- FIXME: not sure why I need this separate function but ok...
_paretoSet :: Query AppData (L.List AppDatum)
_paretoSet = do
  nonempty <- nonemptyDF
  if nonempty
     then do
       p <- rowOne <$> DF.sort paretoOrder `DF.chain` DF.trim 1 
       --DF.filter (not <<< comparable (rowOne p))
       fds <- DF.filter (pointFilter p)
       pure $ p L.: (DF.runQuery _paretoSet fds)
     else pure L.Nil

paretoOrder :: AppDatum -> AppDatum -> Ordering
paretoOrder p1 p2 = 
  if p1' < p2' then GT
  else if p1' > p2' then LT
  else EQ
  where 
  p1' = sum $ SM.values p1
  p2' = sum $ SM.values p2

-- determine if p1 is comparable to p2
comparable :: AppDatum -> AppDatum -> Boolean
comparable p1 p2 = maybe false or $ for (A.union (SM.keys p1) (SM.keys p2)) \k -> do
  v1 <- SM.lookup k p1
  v2 <- SM.lookup k p2
  pure $ v1 < v2

pointFilter :: AppDatum -> AppDatum -> Boolean
pointFilter p1 p2 = (p1 /= p2) && (not $ comparable p1 p2)

nonemptyDF :: Query AppData Boolean
nonemptyDF = do
  rs <- A.length <$> DF.summarize id
  pure $ rs /= 0

rowOne :: AppData -> AppDatum
rowOne = unsafePartial $ fromJust <<< L.head <<< foldMap L.singleton
--rowOne = fromMaybe SM.empty <<< L.head <<< foldMap L.singleton


