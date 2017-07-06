module Test.Util where

import Prelude
import Control.Monad.Aff (Aff)
import Math (abs)
import Test.Spec.Assertions (fail)

shouldApproxEqual :: forall r. Number -> Number -> Number -> Aff r Unit
shouldApproxEqual eps c e = 
  when (abs (c-e) > eps) $
    fail $ (show e) <> " ≉ " <> (show c) <> " (ε = " <> (show eps) <> ")"

