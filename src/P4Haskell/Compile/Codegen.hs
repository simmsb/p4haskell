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
import Data.Unique
import qualified Generics.SOP as GS
import qualified Language.C99.Simple as C
import P4Haskell.Compile.Codegen.Typegen
import P4Haskell.Compile.Declared
import P4Haskell.Compile.Eff
import P4Haskell.Compile.Fetch
import P4Haskell.Compile.Query
import P4Haskell.Compile.Scope
import qualified P4Haskell.Types.AST as AST
import P4Haskell.Utils.Drill
import Polysemy
import Polysemy.Fresh
import Polysemy.Reader
import Polysemy.Writer
import Relude (error)
import Relude.Extra (toFst)

generateMain :: CompC r => Sem r ()
generateMain = do
  main <- fetch GetMain
  let dprsName = main ^. #arguments . ix 2 . #expression . #constructedType . _Typed @AST.TypeName . #path . #name
  controls <- fetch GetTopLevelControl
  let dprs = controls ^?! ix dprsName
  controlName <- generateControl dprs
  tell $ defineFunc "main" (C.TypeSpec C.Void) [] [C.Expr $ C.Funcall (C.Ident controlName) []]
  pure ()

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
          -- TODO: handle extern types (Generate a type for them)
          print $ "Generating Control param: " <> show param
          (ty, _) <- generateP4Type (param ^. #type_)
          let isOut = param ^. #direction . #out
          var <- makeVar (param ^. #name) ty isOut
          let ty' = if isOut then C.Ptr ty else ty
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
