-- |
module P4Haskell.Compile.Codegen.Codegen
    ( generateMain
    , generateControl
     ) where

import Polysemy
import Polysemy.Writer
import Polysemy.Reader
import P4Haskell.Compile.Eff
import P4Haskell.Compile.Declared
import P4Haskell.Compile.Query
import P4Haskell.Compile.Scope
import P4Haskell.Compile.Codegen.Typegen
import qualified P4Haskell.Types.AST as AST
import qualified Language.C99.Simple as C

generateMain :: CompC r => AST.DeclarationInstance -> Sem r ()
generateMain main = undefined


generateControl :: CompC r => AST.P4Control -> Sem r C.Decln
generateControl c = do
  params <- generateControlParams c
  undefined


generateStatement :: CompC r => AST.Statement -> Sem r [C.Stmt] -> Sem r [C.Stmt]
generateStatement (AST.DeclarationVariable'Statement dv) = generateDV dv

generateStatements :: CompC r => [AST.Statement] -> Sem r [C.Stmt]
generateStatements (x : xs) = generateStatement x (generateStatements xs)
generateStatements _        = pure []

generateDV :: CompC r => AST.DeclarationVariable -> Sem r [C.Stmt] -> Sem r [C.Stmt]
generateDV dv rM = do
  ty <- generateP4Type (dv ^. #type_)
  init <- generateP4Expression (dv ^. #initializer)
  var <- makeVar (dv ^. #name) ty
  r <- local (addVarToScope var) rM
  let stmt = undefined
  pure (stmt : r)

generateP4Expression :: CompC r => AST.Expression -> Sem r C.Expr
generateP4Expression = undefined

generateControlParams :: CompC r => AST.P4Control -> Sem r [C.Param]
generateControlParams c = forM
                          (c ^. #type_ . #applyParams . #vec)
                          ( \param -> do
                              ty <- generateP4Type (param ^. #type_)
                              let ty' = if param ^. #direction . #out then C.Ptr ty else ty
                              pure $ C.Param ty' (toString $ param ^. #name)
                          )


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
