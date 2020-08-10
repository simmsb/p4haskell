-- |
module P4Haskell.Compile.Codegen
    (
     ) where

import Polysemy
import P4Haskell.Compile.Eff
import P4Haskell.Compile.Declared
import P4Haskell.Compile.Query
import P4Haskell.Compile.Scope
import qualified P4Haskell.Types.AST as AST

generateMain :: CompC r => AST.DeclarationInstance -> Sem r ()
generateMain main = undefined
