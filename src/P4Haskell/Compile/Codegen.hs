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
import P4Haskell.Compile.Codegen.Utils
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

  let pipeName = main ^. #arguments . ix 1 . #expression . #constructedType . _Typed @AST.TypeName . #path . #name
  let dprsName = main ^. #arguments . ix 2 . #expression . #constructedType . _Typed @AST.TypeName . #path . #name

  controls <- fetch GetTopLevelControl

  let pipeAST = controls ^?! ix pipeName
  pipeControl <- generateControl pipeAST

  let dprsAST = controls ^?! ix dprsName
  dprsControl <- generateControl dprsAST

  modify . (<>) $ defineFunc "main" (C.TypeSpec C.Void) []
    [ C.Stmt . C.Expr $ C.Funcall (C.Ident pipeControl) [] -- TODO: params
    , C.Stmt . C.Expr $ C.Funcall (C.Ident dprsControl) [] -- TODO: params
    ]
  pure ()

generateControl :: CompC r => AST.P4Control -> Sem r C.Ident
generateControl c = do
  (params, vars) <- generateControlParams c
  let localVars = c ^.. #controlLocals . #vec . traverse . _Typed @AST.DeclarationVariable
  let actions = c ^.. #controlLocals . #vec . traverse . _Typed @AST.P4Action
  let scopeUpdate scope =
        let withVars = flipfoldl' addVarToScope scope vars
         in flipfoldl' addActionToScope withVars actions
  body <- local scopeUpdate . generateStatements $ (map injectTyped localVars) <> (c ^. #body . #components)
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
          var <- makeVar (param ^. #name) (C.TypeSpec ty) (param ^. #type_) isOut
          let ty' = if isOut then C.Ptr (C.TypeSpec ty) else (C.TypeSpec ty)
          pure (C.Param ty' (toString $ param ^. #name), var)
      )
