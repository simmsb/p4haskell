-- | Uses [relude](https://hackage.haskell.org/package/relude) as default Prelude.
module Prelude
    ( module Relude
    , module Control.Lens ) where

import           Control.Lens       hiding ( rewrite, transform )

import           Relude             hiding ( Reader, (??), State, execState, runState, error, evalState, gets
                                           , modify, trace, uncons, ask, get, local, runReader )

import Data.Generics.Labels ()
