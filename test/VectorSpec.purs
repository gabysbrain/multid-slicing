module Test.VectorSpec where

import Prelude
import Data.Geom.Vector
import Data.Geom.Point as P
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import Math (sqrt, pi, cos, sin)

import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldContain)

p1 = unsafePartial $ fromJust $ P.fromArray [ 0.0, 0.0 ]
p2 = unsafePartial $ fromJust $ P.fromArray [ 1.0, 1.0 ]
p3 = unsafePartial $ fromJust $ P.fromArray [ 0.0, 1.0 ]
p4 = unsafePartial $ fromJust $ P.fromArray [ 1.0, 0.0 ]
p5 = unsafePartial $ fromJust $ P.fromArray [ 0.5, 0.5 ]

v1 = fromPoints p1 p2
v1' = fromPoints p2 p1
v2 = fromPoints p1 p4
v3 = fromPoints p4 p2
v3' = fromPoints p2 p4

spec :: forall e. Spec e Unit
spec = describe "vectors" do
  describe "length" do
    it "v1 should have length sqrt(2)" do
      len v1 `shouldEqual` (sqrt 2.0)
    it "v2 should have length 1" do
      len v2 `shouldEqual` 1.0
    it "v3 should have length 1" do
      len v3 `shouldEqual` 1.0
  describe "cos theta" do
    it "perpendicular should be 0" do
      cosTheta v2 v3 `shouldEqual` 0.0
    it "isociles 1" do
      cosTheta v1 v2 `shouldEqual` cos (pi/4.0)
    it "isociles 2" do
      cosTheta v1' v3' `shouldEqual` cos (pi/4.0)
  describe "sin theta" do
    it "rhs points should be positive" do
      sinTheta v2 v1 `shouldEqual` sin (pi/4.0)
    it "lhs points should be negative" do
      sinTheta v1 v2 `shouldEqual` -sin (pi/4.0)

