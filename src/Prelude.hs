-- | Uses [relude](https://hackage.haskell.org/package/relude) as default Prelude.
module Prelude
    ( module Relude
    , module Control.Lens
    , module Colog.Polysemy
    , module Polysemy
    , logTextStdout
    , logPPrint
    , logPText ) where

import           Colog.Core.Action  ( LogAction(..) )
import           Colog.Polysemy

import           Control.Lens

import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO
import qualified Data.Text.Lazy.IO  as LTIO

import           Polysemy

import           Relude             hiding ( (??), uncons )

import           Text.Pretty.Simple

logTextStdout :: MonadIO m => LogAction m Text
logTextStdout = LogAction $ liftIO . TIO.putStrLn

logPPrint :: (MonadIO m, Show a) => LogAction m a
logPPrint = LogAction $ liftIO . pPrint

logPText :: MonadIO m => LogAction m Text
logPText = LogAction $ liftIO . LTIO.putStrLn . pString . T.unpack
