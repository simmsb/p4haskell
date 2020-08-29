-- |
module P4Haskell.Compile.Codegen
  ( generateMain
  , generateControl
  )
where

import P4Haskell.Compile.Codegen.Statement
import P4Haskell.Compile.Codegen.Typegen

import Data.Generics.Sum
import Data.Text.Lens (unpacked)
import qualified Language.C99.Simple as C
import P4Haskell.Compile.Declared
import P4Haskell.Compile.Eff
import P4Haskell.Compile.Fetch
import P4Haskell.Compile.Query
import P4Haskell.Compile.Scope
import qualified P4Haskell.Types.AST as AST
import Polysemy
import Polysemy.Reader
import Polysemy.State

generateMain :: CompC r => Sem r ()
generateMain = do
  main <- fetch GetMain
  let dprsName = main ^. #arguments . ix 2 . #expression . #constructedType . _Typed @AST.TypeName . #path . #name
  controls <- fetch GetTopLevelControl
  let dprs = controls ^?! ix dprsName
  controlName <- generateControl dprs
  modify . (<>) $ defineFunc "main" (C.TypeSpec C.Void) []
    [ C.Stmt . C.Expr $ C.Funcall (C.Ident controlName) [] -- TODO: params
    ]
  pure ()

-- HACK: we use (LitInt 0) to signal void expressions
removeDeadExprs :: [C.BlockItem] -> [C.BlockItem]
removeDeadExprs = filter notDeadExpr
  where notDeadExpr (C.Stmt (C.Expr (C.LitInt _))) = False
        notDeadExpr _ = True

generateControl :: CompC r => AST.P4Control -> Sem r C.Ident
generateControl c = do
  (params, vars) <- generateControlParams c
  let scopeUpdate scope = flipfoldl' addVarToScope scope vars
  body <- local scopeUpdate . generateStatements $ c ^. #body . #components
  let body' = removeDeadExprs body
  modify . (<>) $ defineFunc (c ^. #name) (C.TypeSpec C.Void) params body'
  pure $ c ^. #name . unpacked

generateControlParams :: CompC r => AST.P4Control -> Sem r ([C.Param], [Var])
generateControlParams c =
  unzip
    <$> forM
      (c ^. #type_ . #applyParams . #vec)
      ( \param -> do
          (ty, _) <- generateP4Type (param ^. #type_)
          let isOut = param ^. #direction . #out
          var <- makeVar (param ^. #name) (C.TypeSpec ty) isOut
          let ty' = if isOut then C.Ptr (C.TypeSpec ty) else (C.TypeSpec ty)
          pure (C.Param ty' (toString $ param ^. #name), var)
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
