-- |
module P4Haskell.Compile.Codegen.Tables
  ( generateTableCall,
  )
where

import Data.Generics.Sum
import qualified Data.HashMap.Lazy as LH
import Data.Text.Lens (unpacked)
import qualified Generics.SOP as GS
import qualified Language.C99.Simple as C
import P4Haskell.Compile.Codegen.Expression
import P4Haskell.Compile.Codegen.Extern
import {-# SOURCE #-} P4Haskell.Compile.Codegen.MethodCall
import P4Haskell.Compile.Codegen.Statement
import P4Haskell.Compile.Codegen.Typegen
import P4Haskell.Compile.Codegen.Utils
import P4Haskell.Compile.Declared
import P4Haskell.Compile.Eff
import P4Haskell.Compile.Fetch
import P4Haskell.Compile.Query
import P4Haskell.Compile.Scope
import qualified P4Haskell.Types.AST as AST
import P4Haskell.Utils.Drill
import Polysemy
import Polysemy.Reader
import Polysemy.State
import Polysemy.Writer
import Relude (error)

filterMapVec :: (v -> Bool) -> AST.MapVec k v -> AST.MapVec k v
filterMapVec f (AST.MapVec m v) = AST.MapVec (LH.filter f m) (filter f v)

-- TODO: Have params for passing table configurations
-- TODO: Handle each type of table key
-- TODO: Transform table keys into a search trie

-- matchKinds <- fetch GetTopLevelMatchKind
-- let kind = fromJust $ matchKinds ^. at (matchType ^. #name)
-- let ty = case kind ^. #name of
--             "exact"   -> C.TypedefName "_Bool"
--             "ternary" ->

data LiftedAction = LiftedAction
  { nameExpr :: C.Expr,
    liftedParams :: [AST.Parameter],
    liftedParamExprs :: [AST.Expression],
    originalParams :: [AST.Parameter]
  }
  deriving (Generic)

interceptUnknownVars :: CompC r => Sem r a -> Sem r (Scope, a)
interceptUnknownVars = runState emptyScope . intercept @ScopeLookup inner . raise
  where
    inner :: (CompC r, Member (State Scope) r) => forall m x. ScopeLookup m x -> Sem r x
    inner (LookupVarInScope name p4ty) = do
      val <- Polysemy.Reader.asks $ findVarInScope name
      case val of
        Just v -> pure (Just v)
        Nothing -> do
          val' <- Polysemy.State.gets $ findVarInScope name
          case val' of
            Just v -> pure (Just v)
            Nothing -> do
              (ty, _) <- generateP4Type p4ty
              var <- makeVar name (C.TypeSpec ty) p4ty True
              modify $ addVarToScope var
              pure (Just var)

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

generateTableKeys :: (CompC r, Member (Writer [C.BlockItem]) r) => [AST.KeyElement] -> Sem r [(C.Expr, Int)]
generateTableKeys elems =
  mapM
    ( \e -> do
        tmpName <- generateTempVar
        let p4ty = gdrillField @"type_" $ e ^. #expression
        (_, ty) <- generateP4Type p4ty
        expr <- generateP4Expression $ e ^. #expression
        tell [C.Decln $ C.VarDecln Nothing (C.TypeSpec ty) tmpName (Just . C.InitExpr $ expr)]
        let size = case p4ty of
              AST.TypeBits'P4Type (AST.TypeBits l _) -> l
              _ -> error $ "The type: " <> show p4ty <> " is unsupported in table keys"
        pure (C.Ident tmpName, size)
    )
    elems

generateTableCall :: (CompC r, Member (Writer [C.BlockItem]) r) => AST.TypeTable -> Text -> AST.TypeStruct -> Sem r C.Expr
generateTableCall tty name rty = do
  -- XXX: just bad lol
  let rty' = rty & #fields %~ filterMapVec ((/= "action_run") . (^. #name))
  (rty'', _) <- generateP4Type $ AST.TypeStruct'P4Type rty'
  keys <- generateTableKeys $ tty ^. #table . #keys

  pure $ C.LitInt 1
