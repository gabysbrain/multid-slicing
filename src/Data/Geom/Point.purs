module Data.Geom.Point where

import Prelude
import Data.Array ((!!))
import Data.Array as A
import Data.Maybe (Maybe(Just), fromJust)
import Partial.Unsafe (unsafePartial)
import Data.Foldable (sum, foldl)
import Math (sqrt)

newtype Point d = Point (Array Number)

fromArray :: forall d. Array Number -> Maybe (Point d)
fromArray = Just <<< Point

-- TODO: when we have type-length arrays make this origin function
-- origin :: Point d

-- creates a point at zero
zerosLike :: forall d. Point d -> Point d
zerosLike = Point <<< mapDims (const 0.0)

dims :: forall d. Point d -> Int
dims (Point p) = A.length p

sqDist :: forall d. Point d -> Point d -> Number
sqDist (Point p1) (Point p2) = sum $ A.zipWith ((*)) p1 p2
--sqDist (Point p1) (Point p2) = sum $ V.zipWith ((*)) p1 p2

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

