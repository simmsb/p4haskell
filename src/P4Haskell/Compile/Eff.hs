-- |
module P4Haskell.Compile.Eff
  ( CompC,
    runComp,
  )
where

import Data.Unique
import P4Haskell.Compile.Declared
import P4Haskell.Compile.Fetch
import P4Haskell.Compile.Query
import P4Haskell.Compile.Scope
import qualified P4Haskell.Types.AST as AST
import Polysemy
import Polysemy.Fresh
import Polysemy.Reader
import Polysemy.State
import qualified Rock

type CompC r =
  Members
    [ Fetch Query,
      State Declared,
      Reader AST.P4Program,
      ScopeLookup,
      Reader Scope,
      Fresh Unique,
      Embed IO
    ]
    r

runComp ::
  Member (Embed IO) r =>
  Rock.Rules Query ->
  AST.P4Program ->
  Sem
    ( Fetch Query
        ': State Declared
          ': Reader AST.P4Program
            ': ScopeLookup
              ': Reader Scope
                ': Fresh Unique ': r
    )
    a ->
  Sem r (Declared, a)
runComp rules program m = do
  memoVar <- embed $ newIORef mempty

  freshToIO
    . runReader emptyScope
    . runScopeLookupReader
    . runReader program
    . runState mempty
    . runFetchToIO (Rock.runTask (Rock.memoise memoVar rules))
    $ m
