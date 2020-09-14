-- |
module P4Haskell.Compile.Codegen.MethodCall
  ( generateCall,
    generateCall'
  )
where

import qualified Language.C99.Simple as C
import P4Haskell.Compile.Eff
import qualified P4Haskell.Types.AST as AST
import Polysemy
import Polysemy.Writer

generateCall :: (CompC r, Member (Writer [C.BlockItem]) r) => (C.Expr, C.Type) -> [(AST.Parameter, AST.Expression)] -> Sem r C.Expr

generateCall' :: (CompC r, Member (Writer [C.BlockItem]) r) => (Text, C.Type) -> [(Bool, C.Type, AST.Expression)] -> Sem r C.Expr
