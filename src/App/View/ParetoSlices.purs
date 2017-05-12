module App.View.ParetoSlices where

import Prelude hiding (div)
import App.Data (AppData, sortedFieldNames)
import App.Events (Event)
import Data.DataFrame as DF
import Data.List (List)
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Traversable (for_)
import Pareto (paretoSet)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (div, label, h2, h3, button, input, span, ul, li, p)
import Text.Smolder.HTML.Attributes (className, type')
import Text.Smolder.Markup ((!), (#!), text)

view :: AppData -> HTML Event
view = view' <<< DF.runQuery paretoSet

view' :: AppData -> HTML Event
view' paretoPts = 
  div do
    for_ (splomPairs $ sortedFieldNames paretoPts) $ \sr -> do
      div ! className "splom row" $ do
        for_ sr $ \plotFields -> do
          div ! className "splom subplot" $ text (show plotFields)

splomPairs :: forall a. List a -> List (List (Tuple a a))
splomPairs xs = case L.uncons xs of
  Just {head:x,tail:L.Nil} -> L.Nil
  Just {head:x,tail:xs'} -> (map (Tuple x) xs') L.: (splomPairs xs')
  Nothing -> L.Nil

{--pairs :: forall a. List a -> List (Tuple a a)--}
{--pairs xs = fromMaybe L.Nil $ L.zip xs <$> (L.tail xs)--}

