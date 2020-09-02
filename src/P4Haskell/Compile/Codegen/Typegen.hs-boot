-- |
module P4Haskell.Compile.Codegen.Typegen
  ( generateP4Type,
    generateP4TypePure,
    resolveType,
    simplifyType,
    resolveP4Type,
  )
where

import qualified Language.C99.Simple as C
import P4Haskell.Compile.Eff
import P4Haskell.Compile.Query
import qualified P4Haskell.Types.AST as AST
import Polysemy
import qualified Rock

generateP4Type :: CompC r => AST.P4Type -> Sem r (C.TypeSpec, C.TypeSpec)

generateP4TypePure :: Rock.MonadFetch Query m => AST.P4Type -> m (C.TypeSpec, C.TypeSpec, [(Text, C.TypeSpec)])

resolveType :: CompC r => C.TypeSpec -> Sem r C.TypeSpec

simplifyType :: CompC r => C.TypeSpec -> Sem r C.TypeSpec

resolveP4Type :: CompC r => AST.P4Type -> Sem r AST.P4Type
