-- |
module P4Haskell.Compile.Codegen
  ( generateMain,
    generateControl,
  )
where

import Control.Lens
import Data.Generics.Sum
import Data.Text.Lens (unpacked)
import qualified Language.C99.Simple as C
import P4Haskell.Compile.Codegen.Parser
import P4Haskell.Compile.Codegen.Statement
import P4Haskell.Compile.Codegen.Typegen
import P4Haskell.Compile.Codegen.Utils
import P4Haskell.Compile.Declared
import P4Haskell.Compile.Eff
import P4Haskell.Compile.Fetch
import P4Haskell.Compile.Query
import P4Haskell.Compile.Scope
import qualified P4Haskell.Types.AST as AST
import qualified Polysemy as P
import qualified Polysemy.Reader as P
import qualified Polysemy.State as P

generateMain :: CompC r => P.Sem r ()
generateMain = do
  main <- fetch GetMain

  let parseName = main ^. #arguments . ix 0 . #expression . #constructedType . _Typed @AST.TypeName . #path . #name
  let pipeName = main ^. #arguments . ix 1 . #expression . #constructedType . _Typed @AST.TypeName . #path . #name
  let dprsName = main ^. #arguments . ix 2 . #expression . #constructedType . _Typed @AST.TypeName . #path . #name

  parsers <- fetch GetTopLevelParser
  controls <- fetch GetTopLevelControl

  let prsAST = parsers ^?! ix parseName
  prsParser <- generateParser prsAST

  let pipeAST = controls ^?! ix pipeName
  pipeControl <- generateControl pipeAST

  let dprsAST = controls ^?! ix dprsName
  dprsControl <- generateControl dprsAST

  P.modify . (<>) $
    defineFunc
      "main"
      (C.TypeSpec C.Void)
      []
      [ C.Stmt . C.Expr $ C.Funcall (C.Ident prsParser) [],
        C.Stmt . C.Expr $ C.Funcall (C.Ident pipeControl) [], -- TODO: params
        C.Stmt . C.Expr $ C.Funcall (C.Ident dprsControl) [] -- TODO: params
      ]
  pure ()

generateParser :: CompC r => AST.P4Parser -> P.Sem r C.Ident
generateParser p = do
  (params, vars) <- generateParams $ p ^. #type_ . #applyParams . #vec
  let scopeUpdate scope = flipfoldl' addVarToScope scope vars
  body <- P.local scopeUpdate $ generateParserStates (p ^. #name) (p ^. #states)
  let body' = removeDeadExprs body
  P.modify . (<>) $ defineFunc (p ^. #name) (C.TypeSpec C.Void) params body'
  pure $ p ^. #name . unpacked

generateControl :: CompC r => AST.P4Control -> P.Sem r C.Ident
generateControl c = do
  (params, vars) <- generateParams $ c ^. #type_ . #applyParams . #vec
  let localVars = c ^.. #controlLocals . #vec . traverse . _Typed @AST.DeclarationVariable
  let actions = c ^.. #controlLocals . #vec . traverse . _Typed @AST.P4Action
  let scopeUpdate scope =
        let withVars = flipfoldl' addVarToScope scope vars
         in flipfoldl' addActionToScope withVars actions
  body <- P.local scopeUpdate . generateStatements $ map injectTyped localVars <> (c ^. #body . #components)
  let body' = removeDeadExprs body
  P.modify . (<>) $ defineFunc (c ^. #name) (C.TypeSpec C.Bool) params body'
  pure $ c ^. #name . unpacked

generateParams :: CompC r => [AST.Parameter] -> P.Sem r ([C.Param], [Var])
generateParams params =
  unzip
    <$> forM
      params
      ( \param -> do
          (ty, _) <- generateP4Type (param ^. #type_)
          let isOut = param ^. #direction . #out
          var <- makeVar (param ^. #name) (C.TypeSpec ty) (param ^. #type_) isOut
          let ty' = if isOut then C.Ptr (C.TypeSpec ty) else C.TypeSpec ty
          pure (C.Param ty' (toString $ param ^. #name), var)
      )
