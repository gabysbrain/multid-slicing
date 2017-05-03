module App.Data where

import Prelude 
import Control.Monad.Except (Except, except, runExcept, withExcept, mapExcept, throwError)
import Control.Monad.Except.Trans (ExceptT, mapExceptT, withExceptT)
import Data.DataFrame (DataFrame)
import Data.DataFrame as DF
import Data.Either (Either(..), either)
import Data.Foldable (class Foldable)
import Data.List (List)
import Data.List as L
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (class Monoid, mempty)
import Data.Number (fromString)
import Data.NonEmpty as NEL
import Data.Foldable (foldMap, foldr)
import Data.StrMap (StrMap, keys)
import Data.StrMap as SM
import Data.Set (Set)
import Data.Set as S
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Traversable (for)

type AppDatum = StrMap Number
type AppData = DataFrame AppDatum

data CsvError 
  = NoHeaderRow
  | NoDataRows
  | ConvertErr { row :: Int
               , col :: Int
               , message :: String
               }

type CE = Except (NonEmptyList CsvError)

fieldNames :: AppData -> Set String
fieldNames = foldMap (S.fromFoldable <<< keys)

fromCsv :: String -> Except (NonEmptyList CsvError) AppData
fromCsv raw = case L.uncons $ split' (Pattern "\n") raw of
    Nothing -> throwError $ pure NoHeaderRow
    Just {tail:t} | L.length t == 0 -> throwError $ pure NoDataRows
    Just {head:h,tail:t} -> mapExcept (either Left (Right <<< DF.init)) $ 
                              fromCsv' (split' (Pattern ",") h) t

fromCsv' :: List String -> List String -> CE (List (StrMap Number))
fromCsv' keys lines = mergeErrs $
  L.zipWith (\i l -> mapExcept (mapExceptions i) $ procLine l)
            (L.range 1 (L.length lines))
            lines
  where
  mapExceptions i =
    either (\errs -> Left (map (\e -> ConvertErr {row:i,col:fst e,message:snd e}) errs))
           (\vals -> Right (L.singleton $ SM.fromFoldable (L.zip keys vals)))

procLine :: String -> Except (NonEmptyList (Tuple Int String)) (List Number)
procLine line = mergeErrs $ 
  L.zipWith (\i f -> mapExcept (mapExceptions i) $ convertField f)
            (L.range 1 (L.length fields))
            fields
  where
  fields = L.fromFoldable $ split (Pattern ",") line
  mapExceptions i = 
    either (Left <<< pure <<< Tuple i) (Right <<< L.singleton)
    --(L.singleton <<< either (Left <<< pure <<< Tuple i) (Right <<< L.singleton))

convertField :: String -> Except String Number
convertField val = 
  maybe (throwError $ "cannot convert '" <> val <> "' to double") pure $ fromString val

split' p = L.fromFoldable <<< split p

mergeErrs :: forall f n e. Semigroup e => Monoid n => Foldable f => 
             f (Except e n) -> Except e n
mergeErrs = foldr (\e1 e2 -> except $ merge' (runExcept e1) (runExcept e2))
                  (pure mempty)

merge' :: forall m n. Semigroup m => Semigroup n => Either m n -> Either m n -> Either m n
merge' (Left e1) (Left e2) = Left $ e1 <> e2
merge' (Right _) (Left e2) = Left e2
merge' (Left e1) (Right _) = Left e1
merge' (Right v1) (Right v2) = Right $ v1 <> v2

