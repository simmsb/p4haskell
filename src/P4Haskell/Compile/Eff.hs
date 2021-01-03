-- |
module P4Haskell.Compile.Eff (
  CompC,
  runComp,
) where

import Data.Unique
import P4Haskell.Compile.Declared
import P4Haskell.Compile.Fetch
import P4Haskell.Compile.Query
import P4Haskell.Compile.Scope
import qualified P4Haskell.Types.AST as AST
import qualified Polysemy as P
import qualified Polysemy.Fresh as P
import qualified Polysemy.Membership as P
import qualified Polysemy.Reader as P
import qualified Polysemy.State as P
import Relude
import qualified Rock

type CompC r =
  ( P.Members
      [ Fetch Query
      , P.State Declared
      , P.Reader AST.P4Program
      , ScopeLookup
      , P.Reader Scope
      , P.Fresh Unique
      , P.Embed IO
      ]
      r
  , P.KnownRow r
  )

runComp ::
  P.Member (P.Embed IO) r =>
  Rock.Rules Query ->
  AST.P4Program ->
  P.Sem
    ( Fetch Query
        ': P.State Declared
          ': P.Reader AST.P4Program
            ': ScopeLookup
              ': P.Reader Scope
                ': P.Fresh Unique ': r
    )
    a ->
  P.Sem r (Declared, a)
runComp rules program m = do
  memoVar <- P.embed $ newIORef mempty

  P.freshToIO
    . P.runReader emptyScope
    . runScopeLookupReader
    . P.runReader program
    . P.runState mempty
    . runFetchToIO (Rock.runTask (Rock.memoise memoVar rules))
    $ m
