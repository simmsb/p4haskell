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
import Data.Text.Lens (unpacked)
import Relude (error)

generateMain :: CompC r => AST.DeclarationInstance -> Sem r ()
generateMain main = undefined

generateControl :: CompC r => AST.P4Control -> Sem r C.Ident
generateControl c = do
  (params, vars) <- generateControlParams c
  let scopeUpdate scope = flipfoldl' addVarToScope scope vars
  body <- local scopeUpdate . generateStatements $ c ^. #body . #components
  tell $ defineFunc (c ^. #name) (C.TypeSpec C.Void) params body
  pure $ c ^. #name . unpacked

generateControlParams :: CompC r => AST.P4Control -> Sem r ([C.Param], [Var])
generateControlParams c = unzip <$> forM
                          (c ^. #type_ . #applyParams . #vec)
                          ( \param -> do
                              ty <- generateP4Type (param ^. #type_)
                              var <- makeVar (param ^. #name) ty
                              let ty' = if param ^. #direction . #out then C.Ptr ty else ty
                              pure (C.Param ty' (toString $ param ^. #name), var)
                          )

generateStatements :: CompC r => [AST.Statement] -> Sem r [C.Stmt]
generateStatements (x : xs) = generateStatement x (generateStatements xs)
generateStatements _        = pure []

generateStatement :: CompC r => AST.Statement -> Sem r [C.Stmt] -> Sem r [C.Stmt]
generateStatement (AST.DeclarationVariable'Statement dv) = generateDV dv
generateStatement n = liftA2 (<>) (generateStatementInner n)
  where generateStatementInner (AST.AssignmentStatement'Statement as) = generateAS as
        generateStatementInner (AST.IfStatement'Statement is) = generateIS is
        generateStatementInner (AST.MethodCallStatement'Statement ms) = generateMS ms
        generateStatementInner _ = error "unreachable"

generateDV :: CompC r => AST.DeclarationVariable -> Sem r [C.Stmt] -> Sem r [C.Stmt]
generateDV dv rM = do
  ty <- generateP4Type (dv ^. #type_)
  (deps, initExpr) <- runWriterAssocR $ generateP4Expression (dv ^. #initializer)
  var <- makeVar (dv ^. #name) ty
  r <- local (addVarToScope var) rM
  let stmt = C.Var ty (dv ^. #name . unpacked) (Just $ C.InitExpr initExpr)
  pure (deps <> (stmt : r))

generateAS :: CompC r => AST.AssignmentStatement -> Sem r [C.Stmt]
generateAS as = do
  -- TODO: do we need to special case any parts of this?
  (deps, lhs)  <- runWriterAssocR . generateP4Expression $ as ^. #left
  (deps', rhs) <- runWriterAssocR . generateP4Expression $ as ^. #right
  pure (deps <> deps' <> [C.Expr $ C.AssignOp C.Assign lhs rhs])

generateIS :: CompC r => AST.IfStatement -> Sem r [C.Stmt]
generateIS is = do
  (deps, cond) <- runWriterAssocR . generateP4Expression $ is ^. #condition
  ifTrue       <- generateStatements [is ^. #ifTrue]
  ifFalse      <- generateStatements . maybeToList $ is ^. #ifFalse
  pure (deps <> [C.IfElse cond ifTrue ifFalse])

generateMS :: CompC r => AST.MethodCallStatement -> Sem r [C.Stmt]
generateMS ms = do
  (deps, expr) <- runWriterAssocR $ generateP4Expression (AST.MethodCallExpression'Expression $ ms ^. #methodCall)
  pure (deps <> [C.Expr expr])

generateP4Expression :: (CompC r, Member (Writer [C.Stmt]) r) => AST.Expression -> Sem r C.Expr
generateP4Expression (AST.MethodCallExpression'Expression me) = generateME me

generateME :: (CompC r, Member (Writer [C.Stmt]) r) => AST.MethodCallExpression -> Sem r C.Expr
generateME = undefined


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
