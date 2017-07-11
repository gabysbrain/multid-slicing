module Data.Geom.Simplex where

import Prelude
import Data.Set (Set)
import Data.Set as Set
import Data.Array as A
import Data.List (List)
import Data.List as L
import Data.Either (fromRight)
import Data.Maybe (Maybe(..), maybe, fromJust)
import Data.Geom.Point (Point)
import Data.Geom.Point as P
import Data.Geom.Vector (Vector, NormVector)
import Data.Geom.Vector as V
import Data.Geom.Matrix (Matrix)
import Data.Geom.Matrix as M
import Data.Tuple (Tuple(..))
import Data.Foldable (class Foldable, sum)
import Partial.Unsafe (unsafePartial)

newtype Simplex d = Simplex (List (Point d)) -- needs to be d+1 dims
newtype Facet d = Facet  -- basically a hyperplane
  { offset :: Number
  , normal :: NormVector d
  , points :: List (Point d) -- ordering isn't important!
  }

instance eqFacet :: Eq (Facet d) where
  eq (Facet f1) (Facet f2) =
       f1.offset == f2.offset
    && f1.normal == f2.normal
    && f1.points == f2.points

instance showFacet :: Show (Facet d) where
  show (Facet f) = "Facet: " <> show f.offset <> " + " <> show f.points

fromPoints :: forall f d. Foldable f => f (Point d) -> Simplex d
fromPoints = Simplex <<< L.fromFoldable

fromFacet :: forall d. Facet d -> Point d -> Simplex d
fromFacet (Facet f) pt = Simplex $ pt L.: f.points

facets :: forall d. Simplex d -> List (Facet d)
facets (Simplex pts) = map (facet interiorPt) $ combinations dims pts
  where
  -- one day we'll know this from the type :(
  -- TODO: get this from the size-typed vector
  dims = L.length pts
  interiorPt = center pts

points :: forall d. Simplex d -> List (Point d)
points (Simplex pts) = pts

facet :: forall d. Point d -> List (Point d) -> Facet d
facet interiorPt = fixFacetNormal interiorPt <<< _facet

_facet :: forall d. List (Point d) -> Facet d
_facet pts = Facet
    { points: pts
    , offset: _facetOffset pts
    , normal: facetNormal pts
    }

_facetOffset :: forall d. List (Point d) -> Number
_facetOffset pts = maybe 0.0 ((*) (-1.0) <<< V.ptDist norm) $ L.head pts
  where
  norm = facetNormal pts

ridges :: forall d. Facet d -> List (Tuple (Point d) (Point d))
ridges (Facet f) = map toTpl' $ combinations 2 f.points
  where
  toTpl :: Partial => List (Point d) -> Tuple (Point d) (Point d)
  toTpl (L.Cons p1 (L.Cons p2 _)) = Tuple p1 p2
  toTpl' :: List (Point d) -> Tuple (Point d) (Point d)
  toTpl' t = unsafePartial $ toTpl t

-- from https://wiki.haskell.org/99_questions/Solutions/26
combinations :: forall a. Int -> List a -> List (List a)
combinations 0 _    = L.Nil
combinations k vals = case L.uncons vals of
  Just {head:x, tail:xs} -> (map (L.Cons x) (combinations (k-1) xs))
                         <> (combinations k xs)
  Nothing -> L.Nil

aboveFacet :: forall d. Facet d -> Point d -> Boolean
aboveFacet face pt = facetDist face pt > 0.0

facetDist :: forall d. Facet d -> Point d -> Number
facetDist (Facet f) pt = f.offset + (V.ptDist f.normal pt)

center :: forall d.  List (Point d) -> Point d
center = P.centroid

facetVectors :: forall d. List (Point d) -> Matrix
facetVectors L.Nil = M.Matrix []
facetVectors (L.Cons x xs) = M.Matrix $ A.fromFoldable 
                           $ map (V.toArray <<< V.fromPoints x) xs

facetNormal :: forall d. List (Point d) -> NormVector d
facetNormal pts = unsafePartial $ fromJust $ V.norm <$> (M.gaussJ $ facetVectors pts)

fixFacetNormal :: forall d. Point d -> Facet d -> Facet d
fixFacetNormal interiorPoint face@(Facet f) = 
  Facet $ f { normal = if facetDist face interiorPoint > 0.0 
                          then V.mapDims ((*) (-1.0)) f.normal
                          else f.normal
            , offset = if facetDist face interiorPoint > 0.0
                          then (-1.0) * f.offset
                          else f.offset
            }

