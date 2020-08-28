-- |
module P4Haskell.Compile.Codegen.Typegen
  ( generateP4Type,
    generateP4TypePure,
  )
where

import qualified Language.C99.Simple as C
import P4Haskell.Compile.Eff
import P4Haskell.Compile.Query
import qualified P4Haskell.Types.AST as AST
import Polysemy
import qualified Rock

generateP4Type :: CompC r => AST.P4Type -> Sem r (C.Type, C.Type)

generateP4TypePure :: Rock.MonadFetch Query m => AST.P4Type -> m (C.Type, C.Type, [(Text, C.Decln)])
