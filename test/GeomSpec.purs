module Test.GeomSpec where

import Test.Prelude
import App.Data (DataRow(..))
import App.Queries (cosTheta2d)
import Data.Geom.Point (Point)
import Data.Geom.Point as P
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)

pt :: forall d. Array Number -> Point d
pt = unsafePartial $ fromJust <<< P.fromArray

p0 = DataRow {rowId: 0, row: pt [ 0.0, 0.0, 0.0 ] }
p1 = DataRow {rowId: 1, row: pt [ 1.0, 1.0, 0.0 ] }
p2 = DataRow {rowId: 2, row: pt [ 1.0, 1.0, 1.0 ] }
p3 = DataRow {rowId: 3, row: pt [ 0.0, 0.0, 1.0 ] }
--p4 = DataRow {rowId: 4, row: pt [ 1.0, 0.0, 0.0 ] }
--p5 = DataRow {rowId: 5, row: pt [ 0.5, 0.5, 0.0 ] }

l1 = {linkId: 1, src: p0, tgt: p1}
l2 = {linkId: 1, src: p0, tgt: p2}
l3 = {linkId: 1, src: p0, tgt: p3}

spec :: forall e. Spec e Unit
spec = describe "geometry" do
  describe "2D cos theta (angle between vector and 2D projection)" do
    it "is 1 for planar projections" do
      cosTheta2d 0 1 l1 `shouldEqual` 1.0
    it "is 0 for orthogonal projections" do
      cosTheta2d 0 1 l3 `shouldEqual` 0.0
    pending "is between 0 and 1"

