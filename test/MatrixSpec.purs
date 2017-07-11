module Test.MatrixSpec where

import Prelude
import Data.Either (Either(Left,Right))
import Data.Maybe (Maybe(Just,Nothing))
import Data.Geom.Matrix as M
import Data.Geom.Vector as V

import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldContain)

identityM = M.Matrix [[1.0,0.0,0.0],[0.0,1.0,0.0],[0.0,0.0,1.0]]
testM = M.Matrix [[2.0,1.0,-1.0,8.0],[-3.0,-1.0,2.0,-11.0],[-2.0,1.0,2.0,-3.0]]

spec :: forall e. Spec e Unit
spec = describe "Matrix calculations" do
  describe "checking valid matrices" do
    it "identity matrix is valid" do
      M.validMatrix identityM `shouldEqual` true
    it "wikipedia matrix is valid" do
      M.validMatrix testM `shouldEqual` true
--  describe "gauss' function" do
 --   it "identity matrix should work" do
  --    M.gauss' 0 identityM `shouldEqual` Just identityM
   -- it "test matrix should work" do
    --  M.gauss' 0 testM `shouldEqual` Just testM

