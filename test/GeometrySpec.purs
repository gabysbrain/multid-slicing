module Test.GeometrySpec where

import Prelude
import Data.Array as A
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Control.Monad.Aff (Aff)
import Data.Geom.Point as P
import Data.Geom.Vector as V
import Data.Geom.Simplex (_facetOffset, facetVectors)
import Data.Geom.Matrix as M

import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldContain)
import Test.NeighborSpec (createPoint)

isValidMatrix :: forall r. M.Matrix -> Aff r Unit
isValidMatrix c =
    when (not $ isMatrix c) $
      fail $ (show c) <> " not all rows have equal size"
  where
  -- directly from the code
  isMatrix = (==) 1
         <<< A.length
         <<< A.nubBy (\xs ys -> A.length xs == A.length ys)
         <<< unwrap
  
p1 = createPoint [ 0.0, 0.0 ]
p2 = createPoint [ 1.0, 1.0 ]
p3 = createPoint [ 0.0, 1.0 ]
p4 = createPoint [ 1.0, 0.0 ]
p5 = createPoint [ 0.5, 0.5 ]

spec :: forall e. Spec e Unit
spec = describe "geometry calculations" do
  describe "facet matrix creation" do
    it "should create a valid matrix" do
      let pts = L.fromFoldable [p1,p2,p3]
      isValidMatrix $ facetVectors pts
  describe "facet creation" do
    it "facet vectors matrix should have proper size" do
      let pts = L.fromFoldable [p1,p2,p3]
      M.dims (facetVectors pts) `shouldEqual` Tuple 2 2 -- 2 rows because they are vectors
    it "gaussian elimination should work" do
      let pts = L.fromFoldable [p1,p2,p3]
          mtx = facetVectors pts
      M.gaussJ mtx `shouldEqual` (Just $ V.fromPoints p1 p2)
    pending "facet normal should be length 1"
    pending "properly sets the offset" --do
      -- _facetOffset pts `shouldEqual` 0.0

