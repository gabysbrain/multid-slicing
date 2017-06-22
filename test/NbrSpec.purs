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
import Test.Spec.Assertions (fail, shouldEqual, shouldContain)

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
p4 = unsafePartial $ fromJust $ P.fromArray [ 1.0, 0.0 ]
p5 = unsafePartial $ fromJust $ P.fromArray [ 0.5, 0.5 ]

spec :: forall e. Spec e Unit
spec = describe "neighborhood calculation" do
  describe "rhsPts" do
    it "p3 is rhs of p2->p1" do
      rhsPts p2 p1 p3 `shouldEqual` true
    it "p1 is rhs of p3->p2" do
      rhsPts p1 p3 p2 `shouldEqual` true
    it "p4 is rhs of p1->p2" do
      rhsPts p1 p2 p4 `shouldEqual` true
    it "p3 is lhs of p1->p2" do
      rhsPts p1 p2 p4 `shouldEqual` false
    it "rhsPts is > not >=" do
      rhsPts p1 p2 p5 `shouldEqual` false
  describe "splitPts" do
    it "should not include the input points" do
      let split = splitPts p1 p2 (L.fromFoldable [p1,p2,p3])
      split.init `shouldNotContain` p1
      split.init `shouldNotContain` p2
      split.rest `shouldNotContain` p1
      split.rest `shouldNotContain` p2
    it "should properly split based on p1 and p2" do
      let split = splitPts p1 p2 (L.fromFoldable [p1,p2,p3,p4,p5])
      split.init `shouldContain` p4
      split.rest `shouldContain` p3
      split.rest `shouldContain` p5
  describe "quickhull" do
    describe "2 points" do
      it "should always connect" do
        let links = quickhull $ L.fromFoldable [p1,p2]
        links `lengthShouldEqual` 1
    describe "3 points" do
      it "should be fully connected" do
        let links = quickhull $ L.fromFoldable [p1,p2,p3]
        links `lengthShouldEqual` 3
      it "should not have duplicate edges" do
        let links = quickhull $ L.fromFoldable [p1,p2,p3]
        links `shouldEqual` (L.nub links)
    describe "4 points" do
      it "should be fully connected" do
        let links = quickhull $ L.fromFoldable [p1,p2,p3,p4]
        links `lengthShouldEqual` 4
      it "should not have duplicate edges" do
        let links = quickhull $ L.fromFoldable [p1,p2,p3,p4]
        links `shouldEqual` (L.nub links)
    describe "5 points" do
      it "should not connect to the center point" do
        let links = quickhull $ L.fromFoldable [p1,p2,p3,p4,p5]
        links `lengthShouldEqual` 4

