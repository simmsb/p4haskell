-- | Uses [relude](https://hackage.haskell.org/package/relude) as default Prelude.
module Prelude
    ( module Relude
    , module Control.Lens
    , module DiPolysemy
    , module Polysemy ) where

import           Control.Lens       hiding ( rewrite, transform )

import           DiPolysemy

import           Polysemy

import           Relude             hiding ( (??), State, runState, error, evalState, gets
                                           , modify, trace, uncons, get )
