module Data.Geom.Matrix where

-- from https://github.com/emilhaugberg/gaussian-elimination/blob/master/src/Data/Gaussian.purs

import Data.Array (index, length, nubBy, zipWith, snoc, cons)
import Data.Either (Either(..))
import Data.Maybe (Maybe, maybe, fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Prelude (class Eq, class Show, bind, flip, id, join, map, not, pure, show, negate, ($), (*), (+), (-), (/), (<$>), (<#>), (<*>), (<<<), (<>), (==), (=<<), (>>=), (<))
import Data.Geom.Vector (Vector(..))

type    Row    = Array Number
newtype Matrix = Matrix (Array Row)

type Index = Int

derive instance newtypeMatrix :: Newtype Matrix _

instance showMatrix :: Show Matrix where
  show m = "Matrix " <> show (unwrap m)

instance eqMatrix :: Eq Matrix where
  eq m1 m2 = unwrap m1 == unwrap m2

type Length = Int

indexFl :: forall a. Int -> Array a -> Maybe a
indexFl = flip index

subtractRow :: Row -> Row -> Row
subtractRow = zipWith (-)

flipRows :: Row -> Row -> Array Row -> Array Row
flipRows row1 row2 = map (\r -> if r == row1 then row2 else if r == row2 then row1 else id r)

index2 :: forall a. Index -> Index -> Array (Array a) -> Maybe a
index2 i j xs = join $ indexFl j <$> (indexFl i xs)

isZero :: Number -> Boolean
isZero = (==) 0.0

eliminate :: Length -> Index -> Index -> Matrix -> Maybe Matrix
eliminate length k i m = eliminate' m
  where
    eliminate' m = if k == length
      then pure m
      else do
        kElem <- index2  k i m'
        iElem <- index2  i i m'
        kRow  <- indexFl k m'
        iRow  <- indexFl i m'

        let r       = kElem / iElem
        let newKRow = subtractRow (map ((*) r) iRow) kRow
        let newRows = map (\r -> if r == kRow then newKRow else id r) m'

        eliminate length (k + 1) i (Matrix newRows)
    m' = unwrap m

pivot :: Length -> Index -> Index -> Matrix -> Maybe Matrix
pivot length j i m = pivot' m
  where
    pivot' m = if j == length
      then pure m
      else do
        jElem <- index2 j i m'
        if isZero jElem
          then pivot length (j + 1) i m
          else Matrix <$> ((\r1 r2 -> flipRows r1 r2 m') <$> indexFl i m' <*> indexFl j m')
    m' = unwrap m

gauss' :: Index -> Matrix -> Maybe Matrix
gauss' i m =
  if i == n - 1
    then pure m else do
      iElem <- index2 i i rowss
      gauss' (i + 1) =<< eliminate n (i + 1) i =<< if isZero iElem
        then pivot n (i + 1) i m
        else pure m
  where
    rowss = unwrap m
    n     = length rowss

foreign import solver :: Int -> Matrix -> Array Number

gauss :: forall d. Matrix -> Either String (Vector d)
gauss m = if not $ isMatrix m
  then Left "Matrix incorrect"
  else maybe (Left "Couldn't calculate") (Right <<< Vector) result
  where
    result   = solver length' <$> gauss' 0 m
    length'  = (length $ unwrap m)
    isMatrix = (==) 1
           <<< length
           <<< nubBy (\xs ys -> length xs == length ys)
           <<< unwrap
