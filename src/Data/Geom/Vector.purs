module Data.Geom.Vector where

import Prelude
import Data.Geom.Point (Point)
import Data.Geom.Point as P
import Data.Foldable (foldl)
import Math (sqrt)

newtype Vector d = Vector (Array Number)

fromPoints :: forall d. Point d -> Point d -> Vector d
fromPoints src tgt = Vector $ P.zipDimsWith (flip (-)) src tgt

len :: forall d. Vector d -> Number
len = sqrt <<< sqLen

sqLen :: forall d. Vector d -> Number
sqLen (Vector v) = foldl (\s x -> s + x*x) 0.0 v

