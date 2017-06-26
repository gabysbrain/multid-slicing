module Data.Geom.Vector where

import Prelude
import Data.Geom.Point (Point(..))
import Data.Geom.Point as P
import Data.Array as A
import Data.Foldable (foldl, sum)
import Math (sqrt, acos, sin)

newtype Vector d = Vector (Array Number)
type NormVector d = Vector d -- for ensuring len 1

instance eqVector :: Eq (Vector d) where
  eq (Vector v1) (Vector v2) = v1 == v2

fromPoints :: forall d. Point d -> Point d -> Vector d
fromPoints src tgt = Vector $ P.zipDimsWith ((-)) tgt src

len :: forall d. Vector d -> Number
len = sqrt <<< sqLen

sqLen :: forall d. Vector d -> Number
sqLen (Vector v) = foldl (\s x -> s + x*x) 0.0 v

norm :: forall d. Vector d -> NormVector d
norm vv@(Vector v) = Vector $ map (\x -> x / l) v
  where
  l = len vv

cosTheta :: forall d. Vector d -> Vector d -> Number
cosTheta v1@(Vector v1') v2@(Vector v2') =
  (sum $ A.zipWith ((*)) v1' v2') / ((len v1) * (len v2))

sinTheta :: forall d. Vector d -> Vector d -> Number
sinTheta v1 v2 = sqrt $ 1.0 - (cosT*cosT)
  where
  cosT = cosTheta v1 v2

ptDist :: forall d. NormVector d -> Point d -> Number
ptDist (Vector v) (Point p) = sum $ A.zipWith ((*)) v p

toArray :: forall d. Vector d -> Array Number
toArray (Vector v) = v

mapDims :: forall d. (Number -> Number) -> Vector d -> Vector d
mapDims f (Vector v) = Vector $ map f v

