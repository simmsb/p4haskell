-- |
module P4Haskell.Compile.Codegen.Codegen
  ( generateMain,
    generateControl,
  )
where

import Data.Generics.Sum
import Data.Text.Lens (unpacked)
import Data.Unique
import qualified Generics.SOP as GS
import qualified Language.C99.Simple as C
import P4Haskell.Compile.Codegen.Typegen
import P4Haskell.Compile.Declared
import P4Haskell.Compile.Eff
import P4Haskell.Compile.Query
import P4Haskell.Compile.Scope
import qualified P4Haskell.Types.AST as AST
import P4Haskell.Utils.Drill
import Polysemy
import Polysemy.Fresh
import Polysemy.Reader
import Polysemy.Writer
import Relude (error)

fromJustNote :: Text -> Maybe a -> a
fromJustNote _ (Just a) = a
fromJustNote msg _ = error msg

generateTempVar :: Member (Fresh Unique) r => Sem r C.Ident
generateTempVar = do
  i <- hashUnique <$> fresh
  pure $ "tmp_var_" <> show i

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
generateControlParams c =
  unzip
    <$> forM
      (c ^. #type_ . #applyParams . #vec)
      ( \param -> do
          ty <- generateP4Type (param ^. #type_)
          let isOut = param ^. #direction . #out
          var <- makeVar (param ^. #name) ty isOut
          let ty' = if isOut then C.Ptr ty else ty
          pure (C.Param ty' (toString $ param ^. #name), var)
      )

generateStatements :: CompC r => [AST.Statement] -> Sem r [C.Stmt]
generateStatements (x : xs) = generateStatement x (generateStatements xs)
generateStatements _ = pure []

generateStatement :: CompC r => AST.Statement -> Sem r [C.Stmt] -> Sem r [C.Stmt]
generateStatement (AST.DeclarationVariable'Statement dv) = generateDV dv
generateStatement n = liftA2 (<>) (generateStatementInner n)
  where
    generateStatementInner (AST.AssignmentStatement'Statement as) = generateAS as
    generateStatementInner (AST.IfStatement'Statement is) = generateIS is
    generateStatementInner (AST.MethodCallStatement'Statement ms) = generateMS ms
    generateStatementInner _ = error "unreachable"

generateDV :: CompC r => AST.DeclarationVariable -> Sem r [C.Stmt] -> Sem r [C.Stmt]
generateDV dv rM = do
  ty <- generateP4Type (dv ^. #type_)
  (deps, initExpr) <- runWriterAssocR $ generateP4Expression (dv ^. #initializer)
  var <- makeVar (dv ^. #name) ty False
  r <- local (addVarToScope var) rM
  let stmt = C.Var ty (dv ^. #name . unpacked) (Just $ C.InitExpr initExpr)
  pure (deps <> (stmt : r))

generateAS :: CompC r => AST.AssignmentStatement -> Sem r [C.Stmt]
generateAS as = do
  -- TODO: do we need to special case any parts of this?
  (deps, lhs) <- runWriterAssocR . generateP4Expression $ as ^. #left
  (deps', rhs) <- runWriterAssocR . generateP4Expression $ as ^. #right
  pure (deps <> deps' <> [C.Expr $ C.AssignOp C.Assign lhs rhs])

generateIS :: CompC r => AST.IfStatement -> Sem r [C.Stmt]
generateIS is = do
  (deps, cond) <- runWriterAssocR . generateP4Expression $ is ^. #condition
  ifTrue <- generateStatements [is ^. #ifTrue]
  ifFalse <- generateStatements . maybeToList $ is ^. #ifFalse
  pure (deps <> [C.IfElse cond ifTrue ifFalse])

generateMS :: CompC r => AST.MethodCallStatement -> Sem r [C.Stmt]
generateMS ms = do
  (deps, expr) <- runWriterAssocR $ generateP4Expression (AST.MethodCallExpression'Expression $ ms ^. #methodCall)
  pure (deps <> [C.Expr expr])

generateP4Expression :: (CompC r, Member (Writer [C.Stmt]) r) => AST.Expression -> Sem r C.Expr
generateP4Expression (AST.MethodCallExpression'Expression mce) = generateMCE mce
generateP4Expression (AST.Member'Expression me) = generateME me
generateP4Expression (AST.PathExpression'Expression pe) = generatePE pe
generateP4Expression (AST.Constant'Expression ce) = generateCE ce
generateP4Expression (AST.BoolLiteral'Expression ble) = generateBLE ble
generateP4Expression (AST.StringLiteral'Expression sle) = generateSLE sle

generatePE :: CompC r => AST.PathExpression -> Sem r C.Expr
generatePE pe = do
  let ident = C.Ident $ pe ^. #path . #name . unpacked
  -- the ubpf backend YOLOs this too: https://github.com/p4lang/p4c/blob/master/backends/ubpf/ubpfControl.cpp#L262
  var <- Polysemy.Reader.asks $ findVarInScope (pe ^. #path . #name)
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

isCPtr :: C.Type -> Bool
isCPtr (C.Ptr _) = True
isCPtr _ = False

generateME :: (CompC r, Member (Writer [C.Stmt]) r) => AST.Member -> Sem r C.Expr
generateME me = do
  expr <- generateP4Expression $ me ^. #expr
  ty <- generateP4Type . gdrillField @"type_" $ me ^. #expr
  pure $
    if isCPtr ty
      then C.Arrow expr (me ^. #member . unpacked)
      else C.Dot expr (me ^. #member . unpacked)

data MethodType
  = TypeMethod'MethodType AST.TypeMethod
  | TypeAction'MethodType AST.TypeAction
  deriving (Show, Generic, GS.Generic, Eq, Hashable)

processMCEParam :: (CompC r, Member (Writer [C.Stmt]) r)
  => (AST.Parameter, AST.Expression)
  -> Sem r (C.Expr, Maybe C.Stmt)
processMCEParam (p, e) = do
  varName <- generateTempVar
  paramTy <- generateP4Type $ p ^. #type_
  expr <- generateP4Expression e
  if p ^. #direction . #out
    then do
      tmpName <- generateTempVar
      tell
        [ C.Var (C.Ptr paramTy) varName (Just . C.InitExpr $ C.ref expr),
          C.Var paramTy tmpName (Just . C.InitExpr . C.deref $ C.Ident varName)
        ]
      let afterOp = C.Expr $ C.AssignOp C.Assign (C.deref . C.Ident $ varName) (C.Ident tmpName)
      pure (C.ref . C.Ident $ tmpName, Just afterOp)
    else do
      tell [C.Var paramTy varName (Just . C.InitExpr $ expr)]
      pure (C.Ident varName, Nothing)

isCVoid :: C.Type -> Bool
isCVoid (C.TypeSpec C.Void) = True
isCVoid _ = False

generateMCE :: (CompC r, Member (Writer [C.Stmt]) r) => AST.MethodCallExpression -> Sem r C.Expr
generateMCE me = do
  -- TODO: do some type param stuff and overloads for table apply, extern calls, etc
  method <- generateP4Expression . injectSub $ me ^. #method
  resultTy <- generateP4Type $ me ^. #type_
  let methodTy :: MethodType = fromJustNote "Unexpected method type" . projectSub . gdrillField @"type_" $ me ^. #method
  let parameters :: AST.MapVec Text AST.Parameter = gdrillField @"parameters" methodTy

  (params, postStmts) <- unzip <$> mapM processMCEParam (zip (parameters ^. #vec) (me ^.. #arguments . traverse . #expression))

  let postStmts' = catMaybes postStmts

  let callExpr = C.Funcall method params

  if (null postStmts' || isCVoid resultTy)
    then pure callExpr
    else do
      resName <- generateTempVar
      tell [C.Var resultTy resName (Just . C.InitExpr $ callExpr)]
      tell postStmts'
      pure $ C.Ident resName


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
