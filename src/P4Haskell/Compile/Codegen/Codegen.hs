-- |
module P4Haskell.Compile.Codegen.Codegen
    ( generateMain
    , generateControl
     ) where

import Polysemy
import Polysemy.Writer
import P4Haskell.Compile.Eff
import P4Haskell.Compile.Declared
import P4Haskell.Compile.Query
import P4Haskell.Compile.Scope
import P4Haskell.Compile.Codegen.Typegen
import qualified P4Haskell.Types.AST as AST
import qualified P4Haskell.Types.IR  as IR
import qualified Language.C99.Simple as C

generateMain :: CompC r => AST.DeclarationInstance -> Sem r ()
generateMain main = undefined


generateControl :: CompC r => AST.P4Control -> Sem r IR.FunctionDef
generateControl c = do
  let body = c ^. #body . #components
  undefined


generateControlParams :: CompC r => AST.P4Control -> Sem r [IR.Param]
generateControlParams c = do
  let params = c ^. #type_ . #applyParams
  undefined


{-
 NOTE:
 For control blocks:
  1. enumerate through body, performing apply sections
  2. generate backing code for action bodies as required

 need:
   something to lift tables and action into functions
   special case table.apply()
   the struct return value of table.apply() is in the expression result type

-}
