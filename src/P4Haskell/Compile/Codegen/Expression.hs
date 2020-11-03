-- |
module P4Haskell.Compile.Codegen.Expression
  ( generateP4Expression,
  )
where

import Data.Generics.Sum
import Data.Text.Lens (unpacked)
import qualified Generics.SOP as GS
import qualified Language.C99.Simple as C
import P4Haskell.Compile.Codegen.Extern
import P4Haskell.Compile.Codegen.Action
import {-# SOURCE #-} P4Haskell.Compile.Codegen.MethodCall
import {-# SOURCE #-} P4Haskell.Compile.Codegen.Tables
import P4Haskell.Compile.Codegen.Typegen
import P4Haskell.Compile.Eff
import P4Haskell.Compile.Scope
import qualified P4Haskell.Types.AST as AST
import P4Haskell.Utils.Drill
import Polysemy
import Polysemy.Reader
import Polysemy.Writer
import Relude (error)

fromJustNote :: Text -> Maybe a -> a
fromJustNote _ (Just a) = a
fromJustNote msg _ = error msg

generateP4Expression :: (CompC r, Member (Writer [C.BlockItem]) r) => AST.Expression -> Sem r C.Expr
generateP4Expression (AST.MethodCallExpression'Expression mce) = generateMCE mce
generateP4Expression (AST.Member'Expression me) = generateME me
generateP4Expression (AST.PathExpression'Expression pe) = generatePE pe
generateP4Expression (AST.Constant'Expression ce) = generateCE ce
generateP4Expression (AST.BoolLiteral'Expression ble) = generateBLE ble
generateP4Expression (AST.StringLiteral'Expression sle) = generateSLE sle
generateP4Expression (AST.SelectExpression'Expression se) = generateSE se
generateP4Expression (AST.UnaryOp'Expression uoe) = generateUOE uoe
generateP4Expression (AST.BinaryOp'Expression uoe) = generateBOE uoe
generateP4Expression (AST.TypeNameExpression'Expression tn) =
  error $ "type name expressions can only be part of member expressions: " <> show tn

generateUOE :: (CompC r, Member (Writer [C.BlockItem]) r) => AST.UnaryOp -> Sem r C.Expr
generateUOE uoe = do
  expr <- generateP4Expression $ uoe ^. #expr
  pure case uoe ^. #op of
    AST.UnaryOpLNot -> C.UnaryOp C.BoolNot expr

generateBOE :: (CompC r, Member (Writer [C.BlockItem]) r) => AST.BinaryOp -> Sem r C.Expr
generateBOE boe = do
  left <- generateP4Expression $ boe ^. #left
  right <- generateP4Expression $ boe ^. #right
  let op = case boe ^. #op of
        AST.BinaryOpAdd -> C.Add
  pure $ C.BinaryOp op left right

generatePE :: CompC r => AST.PathExpression -> Sem r C.Expr
generatePE pe = do
  let ident = C.Ident $ pe ^. #path . #name . unpacked
  -- the ubpf backend YOLOs this too: https://github.com/p4lang/p4c/blob/master/backends/ubpf/ubpfControl.cpp#L262
  var <- lookupVarInScope (pe ^. #path . #name) (pe ^. #type_)
  let needsDeref = maybe False (^. #needsDeref) var
  pure
    if needsDeref
      then C.deref ident
      else ident

generateCE :: CompC r => AST.Constant -> Sem r C.Expr
generateCE ce = pure . C.LitInt . fromIntegral $ ce ^. #value

generateBLE :: CompC r => AST.BoolLiteral -> Sem r C.Expr
generateBLE ble = pure . C.LitInt $ if ble ^. #value then 1 else 0

generateSLE :: CompC r => AST.StringLiteral -> Sem r C.Expr
generateSLE sle = pure . C.LitString $ sle ^. #value . unpacked

generateME :: (CompC r, Member (Writer [C.BlockItem]) r) => AST.Member -> Sem r C.Expr
generateME (AST.Member _ (AST.TypeNameExpression'Expression tn) n) = do
  (_, ty) <- generateP4Type (tn ^. #type_)
  case ty of
    C.EnumDecln _ (elem $ toString n -> True) -> pure . C.Ident $ toString n
    _ -> error $ "member " <> n <> " not found in: " <> show (tn ^. #typeName)
generateME me = do
  expr <- generateP4Expression $ me ^. #expr
  -- (ty, _) <- generateP4Type . gdrillField @"type_" $ me ^. #expr
  pure $ C.Dot expr (me ^. #member . unpacked)

data MethodType
  = TypeMethod'MethodType AST.TypeMethod
  | TypeAction'MethodType AST.TypeAction
  deriving (Show, Generic, GS.Generic, Eq, Hashable)

data MethodCallType
  = ExternCall Text Text AST.Expression
  | TableCall AST.TypeTable AST.TypeStruct
  | ActionCall Text
  | MethodCall AST.Expression
  deriving (Generic)

decideMethodCallType :: AST.MethodCallExpression -> MethodCallType
decideMethodCallType (AST.MethodCallExpression _ (AST.Member'MethodExpression (AST.Member _ expr member)) _ _)
  | AST.TypeExtern'P4Type ty <- gdrillField @"type_" expr =
    ExternCall (ty ^. #name) member expr
decideMethodCallType (AST.MethodCallExpression (AST.TypeStruct'P4Type rty)
                      (AST.Member'MethodExpression
                       (AST.Member _
                         (AST.PathExpression'Expression
                           (AST.PathExpression (AST.TypeTable'P4Type tty) _))
                         "apply")) _ _) =
  TableCall tty rty
decideMethodCallType (AST.MethodCallExpression (AST.TypeAction'P4Type _)
                      (AST.PathExpression'MethodExpression
                       (AST.PathExpression _ aname)) _ _) = ActionCall (aname ^. #name)
decideMethodCallType (AST.MethodCallExpression _ expr _ _) = MethodCall $ injectSub expr

generateMCE :: (CompC r, Member (Writer [C.BlockItem]) r) => AST.MethodCallExpression -> Sem r C.Expr
generateMCE me = do
  -- TODO: do some type param stuff and overloads for table apply, etc
  case decideMethodCallType me of
    ExternCall name member expr -> do
      (_, expr') <- generateExternCall name member expr (me ^.. #arguments . traverse . #expression)
      pure expr'
    TableCall tty rty -> do
      generateTableCall tty rty
    ActionCall aname -> do
      action' <- lookupActionInScope aname
      case action' of
        Just action -> do
          liftedAction <- liftAction action
          let args = me ^.. #arguments . traverse . #expression
          let params =
                zip (liftedAction ^. #originalParams) args
                  <> zip (liftedAction ^. #liftedParams) (liftedAction ^. #liftedParamExprs)
          generateCall (liftedAction ^. #nameExpr, C.TypeSpec C.Void) params
        Nothing -> error $ "unkown action: " <> aname
    MethodCall expr -> do
      (resultTy, _) <- generateP4Type $ me ^. #type_
      let methodTy :: MethodType = fromJustNote "Unexpected method type" . projectSub . gdrillField @"type_" $ me ^. #method
      let parameters :: AST.MapVec Text AST.Parameter = gdrillField @"parameters" methodTy
      let params = zip (parameters ^. #vec) (me ^.. #arguments . traverse . #expression)
      methodExpr <- generateP4Expression expr
      generateCall (methodExpr, C.TypeSpec resultTy) params

generateSE :: (CompC r, Member (Writer [C.BlockItem]) r) => AST.SelectExpression -> Sem r C.Expr
generateSE se = do
  undefined
