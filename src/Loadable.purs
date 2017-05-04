module Loadable where

data Loadable e a
  = Unloaded
  | Loading
  | Failed e
  | Loaded a

