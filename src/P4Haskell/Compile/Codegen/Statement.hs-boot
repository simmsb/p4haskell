-- |
module P4Haskell.Compile.Codegen.Statement
    ( generateStatements
     ) where

import P4Haskell.Compile.Eff
import qualified Language.C99.Simple as C
import qualified P4Haskell.Types.AST as AST
import Polysemy

generateStatements :: CompC r => [AST.Statement] -> Sem r [C.BlockItem]
