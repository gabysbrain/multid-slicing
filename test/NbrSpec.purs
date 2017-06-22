module Test.NeighborSpec where

import Prelude
import Data.List as L
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import Data.Geom.Point as P
import App.NearestNeighbor (quickhull, splitPts, rhsPts)
import Control.Monad.Aff (Aff())
import Data.Foldable (class Foldable, elem)

import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

shouldNotContain :: forall r f a. Show a => Eq a => Show (f a) => Foldable f 
                 => f a -> a -> Aff r Unit
shouldNotContain c e =
  when (e `elem` c) $
    fail $ (show e) <> " ∈ " <> (show c)

lengthShouldEqual :: forall r a. Show a => L.List a -> Int -> Aff r Unit
lengthShouldEqual xs e =
  when (L.length xs /= e) $
    fail $ "length of " <> (show xs) <> "(" <> (show (L.length xs)) <> ") ≠ " <> (show e)

p1 = unsafePartial $ fromJust $ P.fromArray [ 0.0, 0.0 ]
p2 = unsafePartial $ fromJust $ P.fromArray [ 1.0, 1.0 ]
p3 = unsafePartial $ fromJust $ P.fromArray [ 0.0, 1.0 ]

spec :: forall e. Spec e Unit
spec = describe "neighborhood calculation" do
  describe "rhsPts" do
    it "p3 is rhs of p2->p1" do
      rhsPts p2 p1 p3 `shouldEqual` true
    it "p1 is rhs of p3->p2" do
      rhsPts p1 p3 p2 `shouldEqual` true
  describe "splitPts" do
    it "should not include the input points" do
      let split = splitPts p1 p2 (L.fromFoldable [p1,p2,p3])
      split.init `shouldNotContain` p1
      split.rest `shouldNotContain` p2
  describe "quickhull" do
    it "2 points should always connect" do
      let links = quickhull $ L.fromFoldable [p1,p2]
      links `lengthShouldEqual` 1
    it "3 points should be fully connected" do
      let links = quickhull $ L.fromFoldable [p1,p2,p3]
      links `lengthShouldEqual` 3
    it "3 points not have duplicate edges" do
      let links = quickhull $ L.fromFoldable [p1,p2,p3]
      links `shouldEqual` (L.nub links)

