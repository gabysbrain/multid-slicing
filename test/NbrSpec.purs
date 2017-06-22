module Test.NeighborSpec where

import Prelude

import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: forall e. Spec e Unit
spec = describe "neighborhood calculation" do
  describe "quickhull" do
    pending "2 points should always connect"
    pending "3 points should be fully connected"

