-- | Some monad to operate in while compiling
module P4Haskell.Compile.Fetch
  ( Fetch(..)
  , fetch
  , runFetchToTask
  )
where

import Polysemy
import qualified Rock as R

data Fetch f m a where
  Fetch :: f a -> Fetch f m a

makeSem ''Fetch

runFetchToTask :: Member (Embed (R.Task f)) r => Sem (Fetch f ': r) a -> Sem r a
runFetchToTask = interpret \case
  Fetch f -> embed (R.fetch f)
