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
import Relude (error)
import qualified Rock
import Data.Text.Lens (unpacked)

generateP4Type :: CompC r => AST.P4Type -> Sem r C.Type
generateP4Type t = do
  (ty, deps) <- embedTask $ generateP4TypePure t
  forM_ deps (tell . uncurry declareType)
  pure ty

generateP4TypePure :: Rock.MonadFetch Query m => AST.P4Type -> m (C.Type, [(Text, C.Decln)])
generateP4TypePure (AST.TypeStruct'P4Type s) = generateP4StructPure s
generateP4TypePure (AST.TypeHeader'P4Type s) = generateP4HeaderPure s
generateP4TypePure (AST.TypeEnum'P4Type s) = generateP4EnumPure s
generateP4TypePure (AST.TypeError'P4Type s) = generateP4ErrorPure s
generateP4TypePure (AST.TypeVoid'P4Type s) = generateP4VoidPure s
generateP4TypePure (AST.TypeBits'P4Type s) = generateP4BitsPure s
generateP4TypePure (AST.TypeName'P4Type s) = generateP4TypeNamePure s
generateP4TypePure (AST.TypeBoolean'P4Type s) = generateP4BoolPure s
generateP4TypePure (AST.TypeString'P4Type s) = generateP4StringPure s
generateP4TypePure (AST.TypeTypedef'P4Type s) = generateP4TypeDefPure s
-- generateP4TypePure (AST.TypeParser'P4Type s) = generateP4ParserPure s
-- generateP4TypePure (AST.TypeControl'P4Type s) = generateP4ControlPure s
generateP4TypePure t = error $ "The type: " <> show t <> " shouldn't exist at this point"

generateP4VoidPure :: Rock.MonadFetch Query m => AST.TypeVoid -> m (C.Type, [(Text, C.Decln)])
generateP4VoidPure _ = pure (C.TypeSpec C.Void, [])

generateP4BitsPure :: Rock.MonadFetch Query m => AST.TypeBits -> m (C.Type, [(Text, C.Decln)])
generateP4BitsPure (AST.TypeBits size isSigned) =
  if size `elem` [8, 16, 32, 64]
    then
      let signChar = if isSigned then "" else "u"
       in pure (C.TypeSpec . C.TypedefName $ signChar <> "int" <> show size <> "_t", [])
    else
      let name = "bits_" <> show size
          fields =
            [ C.FieldDecln (C.TypeSpec $ C.TypedefName "uint8_t") ("byte_" <> show i)
              | i <- [0 .. size + 7 `div` 8]
            ]
          struct = C.TypeDecln . C.TypeSpec $ C.StructDecln (Just name) (fromList fields)
       in pure (C.TypeSpec $ C.Struct name, [(toText name, struct)])

generateP4TypeNamePure :: Rock.MonadFetch Query m => AST.TypeName -> m (C.Type, [(Text, C.Decln)])
generateP4TypeNamePure (AST.TypeName p) = do
  type_ <- Rock.fetch $ FetchType (p ^. #name)
  Rock.fetch $ GenerateP4Type type_

generateP4BoolPure :: Rock.MonadFetch Query m => AST.TypeBoolean -> m (C.Type, [(Text, C.Decln)])
generateP4BoolPure AST.TypeBoolean = pure (C.TypeSpec C.Bool, [])

generateP4StringPure :: Rock.MonadFetch Query m => AST.TypeString -> m (C.Type, [(Text, C.Decln)])
generateP4StringPure AST.TypeString = pure (C.Ptr $ C.TypeSpec C.Char, [])

generateP4TypeDefPure :: Rock.MonadFetch Query m => AST.TypeTypedef -> m (C.Type, [(Text, C.Decln)])
generateP4TypeDefPure td = Rock.fetch $ GenerateP4Type (td ^. #type_)
-- NOTE: we silently ignore the name of the typedef here and just return the true name of the type

-- generateP4ParserPure :: (Rock.MonadFetch Query m, MonadIO m) => AST.TypeParser -> m (C.Type, [(Text, C.Decln)])
-- generateP4ParserPure p = do
--   when (null $ p ^. #typeParameters) $
--     print $ "Parser: " <> (p ^. #name) <> " has type parameters that are being ignored"

--   (params, deps) <-
--     SW.runWriterT $
--       forM
--         (p ^. #applyParams . #vec)
--         ( \param -> do
--             (ty, deps) <- Rock.fetch $ GenerateP4Type (param ^. #type_)
--             let ty' = if param ^. #direction . #out then C.Ptr ty else ty
--             SW.tell deps
--             pure $ C.Param ty' (toString $ param ^. #name)
--         )
--   let ident = toString $ p ^. #name
--   let fn = C.FunDecln Nothing (C.TypeSpec C.Void) ident params
--   pure (C.TypeSpec . C.TypedefName $ ident, (p ^. #name, fn) : deps)

-- generateP4ControlPure :: (Rock.MonadFetch Query m, MonadIO m) => AST.TypeControl -> m (C.Type, [(Text, C.Decln)])
-- generateP4ControlPure p = do
--   when (null $ p ^. #typeParameters) $
--     print $ "Control: " <> (p ^. #name) <> " has type parameters that are being ignored"

--   (params, deps) <-
--     SW.runWriterT $
--       forM
--         (p ^. #applyParams . #vec)
--         ( \param -> do
--             (ty, deps) <- Rock.fetch $ GenerateP4Type (param ^. #type_)
--             let ty' = if param ^. #direction . #out then C.Ptr ty else ty
--             SW.tell deps
--             pure $ C.Param ty' (toString $ param ^. #name)
--         )
--   let ident = toString $ p ^. #name
--   let fn = C.FunDecln Nothing (C.TypeSpec C.Void) ident params
--   pure (C.TypeSpec . C.TypedefName $ ident, (p ^. #name, fn) : deps)

generateP4StructPure :: Rock.MonadFetch Query m => AST.TypeStruct -> m (C.Type, [(Text, C.Decln)])
generateP4StructPure s = do
  let fields = s ^. #fields . #vec
  (fields', deps) <-
    SW.runWriterT $
      forM
        fields
        ( \f -> do
            (ty, deps) <- Rock.fetch $ GenerateP4Type (f ^. #type_)
            SW.tell deps
            pure $ C.FieldDecln ty (toString $ f ^. #name)
        )
  let ident = toString $ s ^. #name
  let struct = C.TypeDecln . C.TypeSpec $ C.StructDecln (Just ident) (fromList fields')
  let deps' = ((s ^. #name, struct) : deps)
  pure (C.TypeSpec $ C.Struct ident, deps')

generateP4HeaderPure :: Rock.MonadFetch Query m => AST.TypeHeader -> m (C.Type, [(Text, C.Decln)])
generateP4HeaderPure h = do
  let fields = h ^. #fields . #vec
  (fields', deps) <-
    SW.runWriterT $
      forM
        fields
        ( \f -> do
            (ty, deps) <- Rock.fetch $ GenerateP4Type (f ^. #type_)
            SW.tell deps
            pure $ C.FieldDecln ty (toString $ f ^. #name)
        )
  let ident = toString $ h ^. #name
  let struct = C.TypeDecln . C.TypeSpec $ C.StructDecln (Just ident) (fromList fields')
  let deps' = ((h ^. #name, struct) : deps)
  pure (C.TypeSpec $ C.Struct ident, deps')

generateP4EnumPure :: Rock.MonadFetch Query m => AST.TypeEnum -> m (C.Type, [(Text, C.Decln)])
generateP4EnumPure e =
  let values = e ^.. #members . #vec . traverse . #name . unpacked
      ident = toString $ e ^. #name
      decln = C.TypeDecln . C.TypeSpec $ C.EnumDecln (Just ident) (fromList values)
  in pure (C.TypeSpec $ C.Enum ident, [(e ^. #name, decln)])

generateP4ErrorPure :: Rock.MonadFetch Query m => AST.TypeError -> m (C.Type, [(Text, C.Decln)])
generateP4ErrorPure e =
  let values = e ^.. #members . #vec . traverse . #name . unpacked
      ident = toString $ e ^. #name
      decln = C.TypeDecln . C.TypeSpec $ C.EnumDecln (Just ident) (fromList values)
  in pure (C.TypeSpec $ C.Enum ident, [(e ^. #name, decln)])
