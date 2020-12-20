-- | Some monad to operate in while compiling
module P4Haskell.Compile.Fetch
  ( Fetch (..),
    fetch,
    embedTask,
    runFetchToTask,
    runFetchToIO,
  )
where

import Polysemy
import Polysemy.Embed (runEmbedded)
import Relude
import qualified Rock as R

data Fetch f m a where
  Fetch :: f a -> Fetch f m a
  EmbedTask :: R.Task f a -> Fetch f m a

makeSem ''Fetch

runFetchToTask :: Member (Embed (R.Task f)) r => Sem (Fetch f ': r) a -> Sem r a
runFetchToTask = interpret \case
  Fetch f -> embed (R.fetch f)
  EmbedTask t -> embed t

runFetchToIO :: Member (Embed IO) r => (forall x. R.Task f x -> IO x) -> Sem (Fetch f ': r) a -> Sem r a
runFetchToIO m =
  runEmbedded m . reinterpret \case
    Fetch f -> embed (R.fetch f)
    EmbedTask t -> embed t
