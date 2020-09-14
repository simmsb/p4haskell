-- |
module P4Haskell.Compile.Codegen.MethodCall
  ( generateCall,
    generateCall',
  )
where

import qualified Language.C99.Simple as C
import P4Haskell.Compile.Codegen.Expression
import P4Haskell.Compile.Codegen.Typegen
import P4Haskell.Compile.Codegen.Utils
import P4Haskell.Compile.Eff
import qualified P4Haskell.Types.AST as AST
import Polysemy
import Polysemy.Writer

processParam ::
  (CompC r, Member (Writer [C.BlockItem]) r) =>
  (AST.Parameter, AST.Expression) ->
  Sem r (C.Expr, Maybe C.Stmt)
processParam (p, e) = do
  (paramTy, _) <- generateP4Type $ p ^. #type_
  processParam' (p ^. #direction . #out, C.TypeSpec paramTy, e)

processParam' ::
  (CompC r, Member (Writer [C.BlockItem]) r) =>
  (Bool, C.Type, AST.Expression) ->
  Sem r (C.Expr, Maybe C.Stmt)
processParam' (asRef, paramTy, e) = do
  varName <- generateTempVar
  expr <- generateP4Expression e
  if asRef
    then do
      tmpName <- generateTempVar
      tell
        [ C.Decln $ C.VarDecln Nothing (C.Ptr paramTy) varName (Just . C.InitExpr $ C.ref expr),
          C.Decln $ C.VarDecln Nothing paramTy tmpName (Just . C.InitExpr . C.deref $ C.Ident varName)
        ]
      let afterOp = C.Expr $ C.AssignOp C.Assign (C.deref . C.Ident $ varName) (C.Ident tmpName)
      pure (C.ref . C.Ident $ tmpName, Just afterOp)
    else do
      tell [C.Decln $ C.VarDecln Nothing paramTy varName (Just . C.InitExpr $ expr)]
      pure (C.Ident varName, Nothing)

isCVoid :: C.Type -> Bool
isCVoid (C.TypeSpec C.Void) = True
isCVoid _ = False

generateCall :: (CompC r, Member (Writer [C.BlockItem]) r) => (C.Expr, C.Type) -> [(AST.Parameter, AST.Expression)] -> Sem r C.Expr
generateCall (expr, resultTy) params = do
  (params', postStmts) <- unzip <$> mapM processParam params

  let postStmts' = catMaybes postStmts

  let callExpr = C.Funcall expr params'

  res <-
    if isCVoid resultTy
      then do
        tell [C.Stmt $ C.Expr callExpr]
        pure $ C.LitInt 0
      else do
        resName <- generateTempVar
        tell [C.Decln $ C.VarDecln Nothing resultTy resName (Just . C.InitExpr $ callExpr)]
        pure $ C.Ident resName

  tell $ map C.Stmt postStmts'
  pure res

generateCall' :: (CompC r, Member (Writer [C.BlockItem]) r) => (Text, C.Type) -> [(Bool, C.Type, AST.Expression)] -> Sem r C.Expr
generateCall' (name, resultTy) params = do
  (params', postStmts) <- unzip <$> mapM processParam' params

  let postStmts' = catMaybes postStmts

  let callExpr = C.Funcall (C.Ident $ toString name) params'

  res <-
    if isCVoid resultTy
      then do
        tell [C.Stmt $ C.Expr callExpr]
        pure $ C.LitInt 0
      else do
        resName <- generateTempVar
        tell [C.Decln $ C.VarDecln Nothing resultTy resName (Just . C.InitExpr $ callExpr)]
        pure $ C.Ident resName

  tell $ map C.Stmt postStmts'
  pure res
