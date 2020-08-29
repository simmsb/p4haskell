-- |
module P4Haskell.Compile.Codegen.Statement
    ( generateStatements
     ) where

import Data.Text.Lens (unpacked)
import qualified Language.C99.Simple as C
import P4Haskell.Compile.Codegen.Typegen
import P4Haskell.Compile.Codegen.Expression
import P4Haskell.Compile.Eff
import P4Haskell.Compile.Scope
import qualified P4Haskell.Types.AST as AST
import Polysemy
import Polysemy.Reader
import Polysemy.Writer
import Relude (error)

generateStatements :: CompC r => [AST.Statement] -> Sem r [C.BlockItem]
generateStatements (x : xs) = generateStatement x (generateStatements xs)
generateStatements _ = pure []

generateStatement :: CompC r => AST.Statement -> Sem r [C.BlockItem] -> Sem r [C.BlockItem]
generateStatement (AST.DeclarationVariable'Statement dv) = generateDV dv
generateStatement n = liftA2 (<>) (generateStatementInner n)
  where
    generateStatementInner (AST.AssignmentStatement'Statement as) = generateAS as
    generateStatementInner (AST.IfStatement'Statement is) = generateIS is
    generateStatementInner (AST.MethodCallStatement'Statement ms) = generateMS ms
    generateStatementInner _ = error "unreachable"

generateDV :: CompC r => AST.DeclarationVariable -> Sem r [C.BlockItem] -> Sem r [C.BlockItem]
generateDV dv rM = do
  -- TODO: globals that persist need to be done differently
  (ty, _) <- generateP4Type (dv ^. #type_)
  (deps, initExpr) <- runWriterAssocR $ generateP4Expression (dv ^. #initializer)
  var <- makeVar (dv ^. #name) (C.TypeSpec ty) False
  r <- local (addVarToScope var) rM
  let decln = C.Decln $ C.VarDecln Nothing (C.TypeSpec ty) (dv ^. #name . unpacked) (Just $ C.InitExpr initExpr)
  pure (deps <> (decln : r))

generateAS :: CompC r => AST.AssignmentStatement -> Sem r [C.BlockItem]
generateAS as = do
  -- TODO: do we need to special case any parts of this?
  (deps, lhs) <- runWriterAssocR . generateP4Expression $ as ^. #left
  (deps', rhs) <- runWriterAssocR . generateP4Expression $ as ^. #right
  pure (deps <> deps' <> [C.Stmt . C.Expr $ C.AssignOp C.Assign lhs rhs])

generateIS :: CompC r => AST.IfStatement -> Sem r [C.BlockItem]
generateIS is = do
  (deps, cond) <- runWriterAssocR . generateP4Expression $ is ^. #condition
  ifTrue <- generateStatements [is ^. #ifTrue]
  ifFalse <- generateStatements . maybeToList $ is ^. #ifFalse
  pure (deps <> [C.Stmt $ C.IfElse cond ifTrue ifFalse])

generateMS :: CompC r => AST.MethodCallStatement -> Sem r [C.BlockItem]
generateMS ms = do
  (deps, expr) <- runWriterAssocR $ generateP4Expression (AST.MethodCallExpression'Expression $ ms ^. #methodCall)
  pure (deps <> [C.Stmt $ C.Expr expr])
