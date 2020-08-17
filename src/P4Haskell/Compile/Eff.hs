-- |
module P4Haskell.Compile.Eff
  ( CompC,
  )
where

import qualified P4Haskell.Types.AST as AST
import P4Haskell.Compile.Declared
import P4Haskell.Compile.Fetch
import P4Haskell.Compile.Query
import Polysemy
import Polysemy.Writer
import Polysemy.Reader

type CompC r =
  Members
    [ Fetch Query,
      Writer Declared,
      Reader AST.P4Program
    ]
    r
