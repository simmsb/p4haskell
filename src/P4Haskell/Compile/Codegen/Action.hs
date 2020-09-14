-- |
module P4Haskell.Compile.Codegen.Action
    ( LiftedAction (..)
    , liftAction
     ) where

import qualified Data.HashMap.Lazy as LH
import qualified Language.C99.Simple as C
import {-# SOURCE #-} P4Haskell.Compile.Codegen.Statement
import P4Haskell.Compile.Codegen.Typegen
import P4Haskell.Compile.Codegen.Utils
import P4Haskell.Compile.Declared
import P4Haskell.Compile.Eff
import P4Haskell.Compile.Scope
import qualified P4Haskell.Types.AST as AST
import Polysemy
import Polysemy.Reader
import Polysemy.State

data LiftedAction = LiftedAction
  { nameExpr :: C.Expr,
    liftedParams :: [AST.Parameter],
    liftedParamExprs :: [AST.Expression],
    originalParams :: [AST.Parameter]
  }
  deriving (Generic)

interceptUnknownVars :: CompC r => Sem r a -> Sem r (Scope, a)
interceptUnknownVars m = do
  parentScope <- Polysemy.Reader.ask
  runState emptyScope . intercept @ScopeLookup (inner parentScope) . raise . local (const emptyScope) $ m
  where
    inner :: (CompC r, Member (State Scope) r) => forall m x. Scope -> ScopeLookup m x -> Sem r x
    inner parentScope (LookupVarInScope name p4ty) = do
      val <- Polysemy.Reader.asks $ findVarInScope name
      case val of
        Just v -> pure (Just v)
        Nothing -> do
          val' <- Polysemy.State.gets $ findVarInScope name
          case val' of
            Just v -> pure (Just v)
            Nothing -> do
              case findVarInScope name parentScope of
                Just _ -> do
                  (ty, _) <- generateP4Type p4ty
                  var <- makeVar name (C.TypeSpec ty) p4ty True
                  modify $ addVarToScope var
                  pure (Just var)
                Nothing -> pure Nothing

generateActionParams :: CompC r => AST.P4Action -> Sem r ([C.Param], [Var])
generateActionParams a =
  unzip
    <$> forM
      (a ^. #parameters . #vec)
      ( \param -> do
          (ty, _) <- generateP4Type (param ^. #type_)
          let isOut = param ^. #direction . #out
          var <- makeVar (param ^. #name) (C.TypeSpec ty) (param ^. #type_) isOut
          let ty' = if isOut then C.Ptr (C.TypeSpec ty) else (C.TypeSpec ty)
          pure (C.Param ty' (toString $ param ^. #name), var)
      )

liftAction :: CompC r => AST.P4Action -> Sem r LiftedAction
liftAction action = do
  (params, vars) <- generateActionParams action
  let scopeUpdate scope = flipfoldl' addVarToScope scope vars
  (extraVars, body) <-
    local scopeUpdate . interceptUnknownVars . generateStatements $
      action ^. #body . #components
  let body' = removeDeadExprs body
  let (extraCParams, extraP4Params, paramExprs) =
        unzip3 $ map
          (\v -> ( C.Param (v ^. #varType) (toString $ v ^. #varOriginalName)
                 , AST.Parameter [] (AST.Direction True True) (v ^. #varOriginalName) (v ^. #varP4Type)
                 , AST.PathExpression'Expression $ AST.PathExpression (v ^. #varP4Type) (AST.Path False $ v ^. #varOriginalName)
                 ))
          (LH.elems $ extraVars ^. #scopeVarBindings)
  modify . (<>) $ defineFunc (action ^. #name) (C.TypeSpec C.Void) (params <> extraCParams) body'
  pure $ LiftedAction (C.Ident . toString $ action ^. #name) extraP4Params paramExprs (action ^. #parameters . #vec)
