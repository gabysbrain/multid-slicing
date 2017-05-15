module App.View.ParetoSlices where

import Prelude hiding (div)
import App.Data (AppData, AppDatum, sortedFieldNames)
import App.Events (Event)
import App.View.ParetoVis as PV
import Data.Array as A
import Data.DataFrame as DF
import Data.DataFrame (Query)
import Data.Foldable (foldl)
import Data.List (List)
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe)
import Data.StrMap as SM
import Data.Tuple (Tuple(..), fst, snd)
import Data.Traversable (for_)
import Pareto (ParetoSlab, paretoSet, pareto2dSlabs)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (div, label, h2, h3, button, input, span, ul, li, p)
import Text.Smolder.HTML.Attributes (className, type')
import Text.Smolder.Markup ((!), (#!), text)

view :: Number -> AppData -> HTML Event
view r = view' r <<< DF.runQuery paretoSet

view' :: Number -> AppData -> HTML Event
view' r paretoPts = 
  div do
    for_ (splomPairs $ sortedFieldNames paretoPts) $ \sr -> do
      div ! className "splom row" $ do
        for_ sr $ \plotFields -> do
          div ! className "splom subplot" $ 
            DF.runQuery (paretoPlot r (fst plotFields) (snd plotFields)) paretoPts

paretoPlot :: Number -> String -> String -> Query AppData (HTML Event)
paretoPlot r d1 d2 = do
  limits' <- DF.summarize (extract2d d1 d2)
  let limits = max2d limits'
  paretoData <- pareto2dSlabs r d1 d2 `DF.chain` 
                DF.summarize (extractPts d1 d2)
  pure $ PV.paretoVis (fst limits) (snd limits) paretoData

splomPairs :: forall a. List a -> List (List (Tuple a a))
splomPairs xs = case L.uncons xs of
  Just {head:x,tail:L.Nil} -> L.Nil
  Just {head:x,tail:xs'} -> (map (Tuple x) xs') L.: (splomPairs xs')
  Nothing -> L.Nil

{--pairs :: forall a. List a -> List (Tuple a a)--}
{--pairs xs = fromMaybe L.Nil $ L.zip xs <$> (L.tail xs)--}

extractPts :: String -> String -> ParetoSlab -> Array (Array Number)
extractPts d1 d2 {data:d} = A.catMaybes $ 
                            DF.runQuery (DF.summarize extractPts') d
  where
  extractPts' datum = do
    v1 <- SM.lookup d1 datum
    v2 <- SM.lookup d2 datum
    pure $ [v1, v2]

extract2d :: String -> String -> AppDatum -> Maybe (Tuple Number Number)
extract2d d1 d2 d = do
  v1 <- SM.lookup d1 d
  v2 <- SM.lookup d2 d
  pure $ Tuple v1 v2

max2d :: Array (Maybe (Tuple Number Number)) -> Tuple Number Number
max2d = foldl max' (Tuple 0.0 0.0)
  where
  max' :: Tuple Number Number -> Maybe (Tuple Number Number) -> Tuple Number Number
  max' t Nothing = t
  max' (Tuple x1 y1) (Just (Tuple x2 y2)) = Tuple (max x1 x2) (max y1 y2)

