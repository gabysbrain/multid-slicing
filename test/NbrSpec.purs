module Test.NeighborSpec where

import Prelude
import Data.List as L
import Data.Maybe (fromJust)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Data.Geom.Point as P
import Data.Geom.Simplex as S
import App.NearestNeighbor (quickhull, findHull, initialSimplex)
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
    fail $ "length of " <> (show xs) <> " (len=" <> (show (L.length xs)) <> ") ≠ " <> (show e)

data D2 = D2
data D3 = D3

p1 = createPoint [ 0.0, 0.0 ]
p2 = createPoint [ 1.0, 1.0 ]
p3 = createPoint [ 0.0, 1.0 ]
p4 = createPoint [ 1.0, 0.0 ]
p5 = createPoint [ 0.5, 0.5 ]

spec :: forall e. Spec e Unit
spec = describe "neighborhood calculation" do
  describe "quickhull" do
    describe "2 2D points" do
      it "should not connect" do
        let links = quickhull $ L.fromFoldable [p1,p2]
        links `lengthShouldEqual` 0
    describe "3 2D points" do
      it "initial simplex should use the points" do
        let (Tuple simplex xs) = initialSimplex $ L.fromFoldable [p1,p2,p3]
        -- FIXME: fragile test!
        S.points simplex `shouldContain` p1
        S.points simplex `shouldContain` p2
        S.points simplex `shouldContain` p3
      it "initial simplex should not return extra points" do
        let (Tuple simplex xs) = initialSimplex $ L.fromFoldable [p1,p2,p3]
        xs `lengthShouldEqual` 0
      it "should be fully connected" do
        let links = quickhull $ L.fromFoldable [p1,p2,p3]
        links `lengthShouldEqual` 3
      it "should not have duplicate edges" do
        let links = quickhull $ L.fromFoldable [p1,p2,p3]
        links `shouldEqual` (L.nub links)
    describe "4 2D points" do
      it "should be fully connected" do
        let links = quickhull $ L.fromFoldable [p1,p2,p3,p4]
        links `lengthShouldEqual` 4
      it "should not have duplicate edges" do
        let links = quickhull $ L.fromFoldable [p1,p2,p3,p4]
        links `shouldEqual` (L.nub links)
    describe "5 2D points" do
      it "should not connect to the center point" do
        let links = quickhull $ L.fromFoldable [p1,p2,p3,p4,p5]
        links `lengthShouldEqual` 4
  describe "findHull" do
    it "with no points should give back the facet" do
      let facet = S.facet p1 (L.fromFoldable [p2,p3])
      findHull facet L.Nil `shouldEqual` L.singleton facet

createPoint :: forall d. Array Number -> P.Point d
createPoint pts = unsafePartial $ fromJust $ P.fromArray pts

