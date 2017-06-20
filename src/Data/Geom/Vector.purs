module Data.Geom.Vector where

import Prelude
import Data.Geom.Point (Point)
import Data.Geom.Point as P
import Data.Array as A
import Data.Foldable (foldl, sum)
import Math (sqrt, acos, sin)

newtype Vector d = Vector (Array Number)

fromPoints :: forall d. Point d -> Point d -> Vector d
fromPoints src tgt = Vector $ P.zipDimsWith ((-)) tgt src

len :: forall d. Vector d -> Number
len = sqrt <<< sqLen

sqLen :: forall d. Vector d -> Number
sqLen (Vector v) = foldl (\s x -> s + x*x) 0.0 v

-- FIXME: should take a vector!
dist2pt :: forall d. Point d -> Point d -> Point d -> Number
dist2pt lp1 lp2 pt = (sinTheta l1 l2) * (len l2)
  where
  l1 = fromPoints lp1 lp2
  l2 = fromPoints pt  lp2

cosTheta :: forall d. Vector d -> Vector d -> Number
cosTheta v1@(Vector v1') v2@(Vector v2') =
  (sum $ A.zipWith ((*)) v1' v2') / ((len v1) * (len v2))

sinTheta :: forall d. Vector d -> Vector d -> Number
sinTheta v1 v2 = sin $ acos $ cosTheta v1 v2

