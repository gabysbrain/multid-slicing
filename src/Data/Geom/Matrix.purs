module Data.Geom.Matrix where

import Prelude 
import Data.Array (index, length, updateAt, find, take, drop, head, tail, init, last, uncons, unsnoc, nubBy, zipWith, filter, (!!), (..), (:))
import Data.Either (Either(..))
import Data.Foldable (sum, foldl, foldr, foldM)
import Data.Traversable (traverse, for)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..), fst, snd)
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

dims :: Matrix -> Tuple Int Int
dims (Matrix rs) = case uncons rs of
  Nothing -> Tuple 0 0
  Just {head:c1,tail:cs} -> Tuple (length cs + 1) (length c1)

rows :: Matrix -> Int
rows = fst <<< dims

cols :: Matrix -> Int
cols = snd <<< dims

cell :: Index -> Index -> Matrix -> Maybe Number
cell r c m = do
  row <- (unwrap m) !! r
  col <- row !! c
  pure col

validMatrix :: Matrix -> Boolean
validMatrix = (==) 1
          <<< length
          <<< nubBy (\xs ys -> length xs == length ys)
          <<< unwrap

splitAt :: forall a. Int -> Array a -> Tuple (Array a) (Array a)
splitAt i xs = Tuple (take i xs) (drop i xs)

--swaps element at position a with element at position b.
swap :: Matrix -> Index -> Index -> Maybe Matrix
swap m a b | a == b = pure m
swap m a b          = do
  r1 <- (unwrap m) !! a
  r2 <- (unwrap m) !! b
  m' <- (updateAt a r2 (unwrap m) >>= updateAt b r1)
  pure $ Matrix m'

-- Gaussian elimination operation, subtract and multiply
factorSub :: Number -> Row -> Row -> Row
factorSub k = zipWith (\x y -> k*x - y)

reduceRow :: Matrix -> Index -> Maybe Matrix
reduceRow m r = do
    -- index of first non-zero element on or below (r,r).
    firstnonzero <- find (\x -> maybe false ((/=) 0.0) $ cell x r m)
                         (r..(rows m-1))
 
    --matrix with row swapped (if needed)
    m' <- swap m r firstnonzero
 
    --row we're working with
    pivot <- (unwrap m) !! r
 
    --make it have 1 as the leading coefficient
    f <- pivot !! r
    let pivot' = map (\x -> x / f) pivot
 
    --apply subrow to all rows below
    let rs = drop (r+1) $ unwrap m'
    nextrows <- for (drop (r+1) $ unwrap m') $ \nr -> do
      k <- nr !! r
      pure $ factorSub k pivot' nr
 
    -- concat the lists and repeat
    pure $ Matrix $ take r (unwrap m') <> [pivot'] <> nextrows

-- from https://luckytoilet.wordpress.com/2010/02/21/solving-systems-of-linear-equations-in-haskell/
gauss :: Matrix -> Maybe Matrix
gauss matrix = foldM reduceRow matrix (0..(rows matrix-1)) >>= fixlastrow
  where
  fixlastrow :: Matrix -> Maybe Matrix
  fixlastrow (Matrix m') = do
    a <- init m'
    rw <- last m'
    z <- last rw
    nz <- last =<< init rw
    rst <- init =<< init rw
    pure $ Matrix $ a <> [rst <> [1.0, z / nz]]

--Solve a matrix (must already be in REF form) by back substitution.
substitute :: forall d. Matrix -> Maybe (Vector d)
substitute matrix = do
    sMtx <- unsnoc $ unwrap matrix
    solution1 <- last sMtx.last
    solution <- foldrM next [solution1] sMtx.init
    pure $ Vector solution
  where
  foldrM :: forall a b m. Monad m => (a -> b -> m b) -> b -> Array a -> m b
  foldrM f d xs = case uncons xs of
    Nothing -> pure d
    Just spt -> (\x -> f spt.head x) =<< foldrM f d spt.tail
  next :: Row -> Array Number -> Maybe (Array Number)
  next row found = do
    subpart <- init $ drop (rows matrix - length found) row
    lastElem <- last row
    let solution = lastElem - sum (zipWith (*) found subpart)
    pure $ solution : found

gaussJ :: forall d. Matrix -> Maybe (Vector d)
gaussJ m = gauss m >>= substitute
 
