module Data.Geom.Line where

import Prelude
import Data.Geom.Point (Point)
import Data.Geom.Vector (Vector)
import Data.Geom.Vector as V

newtype Line d = Line {src::Point d, tgt::Point d}

fromPoints :: forall d. Point d -> Point d -> Line d
fromPoints p1 p2 = Line {src:p1,tgt:p2}

asVector :: forall d. Line d -> Vector d
asVector (Line l) = V.fromPoints l.src l.tgt

dist2pt :: forall d. Line d -> Point d -> Number
dist2pt ll@(Line l) pt = (V.sinTheta l1 l2) * (V.len l2)
  where
  l1 = asVector ll
  l2 = V.fromPoints pt l.src

