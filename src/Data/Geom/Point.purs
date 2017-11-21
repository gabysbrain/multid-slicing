module Data.Geom.Point where

import Prelude
import Data.Array ((!!))
import Data.Array as A
import Data.Int (toNumber)
import Data.List (List)
import Data.List as L
import Data.Maybe (Maybe(Just), fromJust)
import Partial.Unsafe (unsafePartial)
import Data.Foldable (sum, foldl)
import Math (sqrt)

newtype Point d = Point (Array Number)

instance eqPoint :: Eq (Point d) where
  eq (Point p1) (Point p2) = p1 == p2

instance showPoint :: Show (Point d) where
  show (Point p) = show p

fromArray :: forall d. Array Number -> Maybe (Point d)
fromArray = Just <<< Point

toArray :: forall d. Point d -> Array Number
toArray (Point p) = p

-- TODO: when we have type-length arrays make this origin function
-- origin :: Point d

-- creates a point at zero
zerosLike :: forall d. Point d -> Point d
zerosLike = Point <<< mapDims (const 0.0)

dims :: forall d. Point d -> Int
dims (Point p) = A.length p

sqDist :: forall d. Point d -> Point d -> Number
sqDist (Point p1) (Point p2) = sum $ A.zipWith (\x1 x2 -> (x1-x2)*(x1-x2)) p1 p2

dist :: forall d. Point d -> Point d -> Number
dist p1 p2 = sqrt $ sqDist p1 p2

fold :: forall a d. (a -> Number -> a) -> a -> Point d -> a
fold f init (Point v) = foldl f init v

-- project a point into 2D
project2D :: forall d d'. Int -> Int -> Point d -> Point d'
project2D d1 d2 p = Point $ [p !!! d1, p !!! d2]

-- project a point into everything but the 2 given dimensions
projectNot2D :: forall d d'. Int -> Int -> Point d -> Point d'
projectNot2D d1 d2 p = del p
  where
  del = if d1 < d2 
           then delAt d1 <<< delAt d2
           else delAt d2 <<< delAt d1
--project' d1 d2 (Point p) = Point $ delAt d1 $ delAt d2 p

projectNot :: forall d d'. Int -> Point d -> Point d'
projectNot d1 p = delAt d1 p

projectUp :: forall d d'. (Array Number -> Array Number) -> Point d -> Point d'
projectUp f (Point p) = Point $ p <> f p

centroid :: forall d. List (Point d) -> Point d
centroid L.Nil = Point $ [0.0]
centroid pts@(L.Cons pt _) = Point $ mapDims (\x -> x / len) ttl
  where
  ttl = foldl (\s p -> Point $ zipDimsWith ((+)) s p) (zerosLike pt) pts
  len = toNumber $ L.length pts

mapDims :: forall a d
         . (Number -> a) -> Point d -> Array a
mapDims f (Point p) = map f p

zipDimsWith :: forall a d
             . (Number -> Number -> a) -> Point d -> Point d -> Array a
zipDimsWith f (Point p1) (Point p2) = A.zipWith f p1 p2

-- mapping things
idx :: forall d. Point d -> Int -> Number
idx (Point p) i = unsafePartial $ fromJust $ p !! i
infixl 8 idx as !!!

delAt :: forall d d'. Int -> Point d -> Point d'
delAt i (Point p) = Point $ unsafePartial $ fromJust $ A.deleteAt i p

updateAt :: forall d. Int -> Number -> Point d -> Point d
updateAt i x (Point p) = Point $ unsafePartial $ fromJust $ A.updateAt i x p

