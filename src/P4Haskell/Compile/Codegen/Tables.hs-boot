-- |
module P4Haskell.Compile.Codegen.Tables
  ( generateTableCall,
  )
where

import qualified Language.C99.Simple as C
import P4Haskell.Compile.Eff
import qualified P4Haskell.Types.AST as AST
import Polysemy
import Polysemy.Writer

generateTableCall :: (CompC r, Member (Writer [C.BlockItem]) r) => AST.TypeTable -> AST.TypeStruct -> Sem r C.Expr
