-- |
module P4Haskell.Compile.Codegen.Typegen
  ( generateP4Type,
    generateP4TypePure,
    generateP4StructPure,
  )
where

import qualified Control.Monad.Writer.Strict as SW
import qualified Language.C99.Simple as C
import P4Haskell.Compile.Declared
import P4Haskell.Compile.Eff
import P4Haskell.Compile.Fetch
import P4Haskell.Compile.Query
import qualified P4Haskell.Types.AST as AST
import Polysemy
import Polysemy.Writer
import qualified Rock
import Relude (error)

generateP4Type :: CompC r => AST.P4Type -> Sem r C.TypeSpec
generateP4Type t = do
  (ty, deps) <- embedTask $ generateP4TypePure t
  forM_ deps (tell . uncurry declareType)
  pure ty

generateP4TypePure :: Rock.MonadFetch Query m => AST.P4Type -> m (C.TypeSpec, [(Text, C.TypeSpec)])
generateP4TypePure (AST.TypeStruct'P4Type  s) = generateP4StructPure s
generateP4TypePure (AST.TypeVoid'P4Type    s) = generateP4VoidPure s
generateP4TypePure (AST.TypeBits'P4Type    s) = generateP4BitsPure s
generateP4TypePure (AST.TypeName'P4Type    s) = generateP4TypeNamePure s
generateP4TypePure (AST.TypeBoolean'P4Type s) = generateP4BoolPure s
generateP4TypePure t = error $ "The type: " <> show t <> " shouldn't exist at this point"

generateP4VoidPure :: Rock.MonadFetch Query m => AST.TypeVoid -> m (C.TypeSpec, [(Text, C.TypeSpec)])
generateP4VoidPure _ = pure (C.Void, [])

generateP4BitsPure :: Rock.MonadFetch Query m => AST.TypeBits -> m (C.TypeSpec, [(Text, C.TypeSpec)])
generateP4BitsPure (AST.TypeBits size isSigned) =
  if size `elem` [8, 16, 32, 64] then
    let signChar = if isSigned then "" else "u"
    in pure (C.TypedefName $ signChar <> "int" <> show size <> "_t", [])
  else
    let name = "bits_" <> show size
        fields = [C.FieldDecln (C.TypeSpec $ C.TypedefName "uint8_t") ("byte_" <> show i)
                 | i <- [0 .. size + 7 `div` 8]]
        struct = C.StructDecln (Just name) fields
    in pure (C.Struct name, [(toText name, struct)])

generateP4TypeNamePure :: Rock.MonadFetch Query m => AST.TypeName -> m (C.TypeSpec, [(Text, C.TypeSpec)])
generateP4TypeNamePure (AST.TypeName p) = do
  type_ <- Rock.fetch $ FetchType (p ^. #name)
  Rock.fetch $ GenerateP4Type type_

generateP4BoolPure :: Rock.MonadFetch Query m => AST.TypeBoolean -> m (C.TypeSpec, [(Text, C.TypeSpec)])
generateP4BoolPure AST.TypeBoolean = pure (C.Bool, [])

generateP4StructPure :: Rock.MonadFetch Query m => AST.TypeStruct -> m (C.TypeSpec, [(Text, C.TypeSpec)])
generateP4StructPure s = do
  let fields = s ^. #fields . #vec
  (fields', deps) <-
    SW.runWriterT $
      forM
        fields
        ( \f -> do
            (ty, deps) <- Rock.fetch $ GenerateP4Type (f ^. #type_)
            SW.tell deps
            pure $ C.FieldDecln (C.TypeSpec ty) (toString $ f ^. #name)
        )
  let ident = toString $ s ^. #name
  let struct = C.StructDecln (Just ident) fields'
  let deps' = ((s ^. #name, struct) : deps)
  pure (C.Struct ident, deps')
