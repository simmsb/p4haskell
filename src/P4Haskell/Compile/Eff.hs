-- |
module P4Haskell.Compile.Eff
  ( CompC,
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
import Polysemy.Writer

type CompC r =
  Members
    [ Fetch Query,
      Writer Declared,
      Reader AST.P4Program,
      Reader Scope,
      Fresh Unique
    ]
    r
