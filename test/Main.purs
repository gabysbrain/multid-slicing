module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.FS (FS)
import Test.Spec.Discovery (discover)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

type TestEffects = RunnerEffects (fs::FS, exception::EXCEPTION)

main :: Eff TestEffects Unit
main = discover "Test\\..*Spec" 
  >>= run 
    [ consoleReporter
    ]

