-- | Uses [relude](https://hackage.haskell.org/package/relude) as default Prelude.
module Prelude
    ( module Relude
    , module Control.Lens
    , module DiPolysemy
    , module Polysemy ) where

import           Control.Lens       hiding ( rewrite, transform )

import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO
import qualified Data.Text.Lazy.IO  as LTIO

import           DiPolysemy

import           Polysemy

import           Relude             hiding ( (??), State, error, evalState, gets
                                           , modify, trace, uncons, get )

import           Text.Pretty.Simple
