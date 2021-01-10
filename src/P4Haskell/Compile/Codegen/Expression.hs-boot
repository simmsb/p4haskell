-- |
module P4Haskell.Compile.Codegen.Expression (
  generateP4Expression,
) where

import qualified Language.C99.Simple as C
import P4Haskell.Compile.Eff
import qualified P4Haskell.Types.AST as AST
import qualified Polysemy as P
import qualified Polysemy.Writer as P

generateP4Expression :: (CompC r, P.Member (P.Writer [C.BlockItem]) r) => AST.Expression -> P.Sem r C.Expr
