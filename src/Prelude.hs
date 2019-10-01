-- | Uses [relude](https://hackage.haskell.org/package/relude) as default Prelude.
module Prelude
    ( module Relude
    , module Control.Lens
    , module Colog.Polysemy
    , module Polysemy
    , logTextStdout ) where

import           Colog.Core.Action ( LogAction(..) )
import           Colog.Polysemy

import           Control.Lens

import qualified Data.Text.IO      as TIO

import           Polysemy

import           Relude            hiding ( (??), uncons )

logTextStdout :: MonadIO m => LogAction m Text
logTextStdout = LogAction $ liftIO . TIO.putStrLn
