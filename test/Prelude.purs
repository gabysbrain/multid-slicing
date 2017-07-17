module Test.Prelude 
  ( module Prelude
  , module Test.Spec
  , module Test.Assertions
  ) where

import Prelude
import Test.Spec (Spec, pending, describe, it)
import Test.Assertions

