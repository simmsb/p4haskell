-- |
module P4Haskell.Compile.Codegen.Extern
  ( generateExternCall,
    getExternType,
  )
where

import qualified Language.C99.Simple as C
import {-# SOURCE #-} P4Haskell.Compile.Codegen.MethodCall
import {-# SOURCE #-} P4Haskell.Compile.Codegen.Typegen
import P4Haskell.Compile.Declared
import P4Haskell.Compile.Eff
import qualified P4Haskell.Types.AST as AST
import P4Haskell.Utils.Drill
import Polysemy
import Polysemy.State
import Polysemy.Writer
import Relude (error)
import Relude.Extra (toFst)

data ExternInfo = ExternInfo
  { name :: Text,
    type_ :: C.TypeSpec,
    methods :: HashMap Text ExternMethod
  }
  deriving (Generic)

data ExternMethod = ExternMethod
  { name :: Text,
    generate ::
      forall r.
      (Member (Writer [C.BlockItem]) r, CompC r) =>
      AST.Expression ->
      [AST.Expression] ->
      Sem r (C.Type, C.Expr)
  }

uint32_t :: C.TypeSpec
uint32_t = C.TypedefName "uint32_t"

packetStruct :: C.TypeSpec
packetStruct =
  C.StructDecln
    (Just "packet")
    ( fromList
        [ C.FieldDecln (C.TypeSpec uint32_t) "idx",
          C.FieldDecln (C.Ptr . C.TypeSpec $ C.Char) "pkt"
        ]
    )

externs :: HashMap Text ExternInfo
externs =
  fromList . map (toFst (^. #name)) $
    [ ExternInfo "packet_out" packetStruct packetOutMethods,
      ExternInfo "packet_in" packetStruct packetInMethods
    ]

r :: (r -> a) -> r -> a
r f = f

packetOutMethods :: HashMap Text ExternMethod
packetOutMethods =
  fromList . map (toFst $ r @ExternMethod name) $
    [ ExternMethod "emit" generatePacketOutEmit
    ]

packetInMethods :: HashMap Text ExternMethod
packetInMethods =
  fromList . map (toFst $ r @ExternMethod name) $
    [ ExternMethod "extract" generatePacketInExtract,
      ExternMethod "lookahead" generatePacketInLookahead,
      ExternMethod "advance" generatePacketInAdvance,
      ExternMethod "length" generatePacketInLength
    ]

generatePacketInExtract :: (Member (Writer [C.BlockItem]) r, CompC r) => AST.Expression -> [AST.Expression] -> Sem r (C.Type, C.Expr)
generatePacketInExtract instance_ params = do
  undefined

generatePacketInLookahead :: (Member (Writer [C.BlockItem]) r, CompC r) => AST.Expression -> [AST.Expression] -> Sem r (C.Type, C.Expr)
generatePacketInLookahead instance_ params = do
  undefined

generatePacketInAdvance :: (Member (Writer [C.BlockItem]) r, CompC r) => AST.Expression -> [AST.Expression] -> Sem r (C.Type, C.Expr)
generatePacketInAdvance instance_ params = do
  undefined

generatePacketInLength :: (Member (Writer [C.BlockItem]) r, CompC r) => AST.Expression -> [AST.Expression] -> Sem r (C.Type, C.Expr)
generatePacketInLength instance_ params = do
  undefined

findFields :: CompC r => C.TypeSpec -> Sem r [(C.Expr -> C.Expr, Integer)]
findFields (C.StructDecln _ fields) = (concat <$>) . mapM processField $ toList fields
  where
    processField :: CompC r => C.FieldDecln -> Sem r [(C.Expr -> C.Expr, Integer)]
    processField (C.FieldDecln (C.TypeSpec ty) ident) = do
      ty' <- resolveType ty
      fields' <- findFields ty'
      pure $ map (updateField ident) fields'
    processField (C.FieldDecln ty _) = error $ "I don't know how to generate an accessor for the type: " <> show ty
    updateField name (access, size) = ((\e -> access $ C.Dot e name), size)
findFields (C.TypedefName "uint8_t") = pure [(id, 1)]
findFields (C.TypedefName "uint16_t") = pure [(id, 2)]
findFields (C.TypedefName "uint32_t") = pure [(id, 3)]
findFields (C.TypedefName "uint64_t") = pure [(id, 4)]
findFields (C.TypedefName "int8_t") = pure [(id, 1)]
findFields (C.TypedefName "int16_t") = pure [(id, 2)]
findFields (C.TypedefName "int32_t") = pure [(id, 3)]
findFields (C.TypedefName "int64_t") = pure [(id, 4)]
findFields t = error $ "unknown type for findFields: " <> show t

generateOutEmitBody :: CompC r => C.TypeSpec -> Sem r [C.BlockItem]
generateOutEmitBody ty = concatMap generateWrite <$> findFields ty
  where
    generateWrite (acc, size) =
      [ C.Stmt . C.Expr $
          C.Funcall
            (C.Ident "memcpy")
            [ (C.ref $ C.Arrow (C.Ident "pkt") "pkt") C..+ C.Arrow (C.Ident "pkt") "idx",
              C.ref . acc . C.deref $ C.Ident "value",
              C.LitInt size
            ],
        C.Stmt $ C.Expr (C.Arrow (C.Ident "pkt") "idx" C..+= C.LitInt size)
      ]

getTypeName :: AST.P4Type -> Text
getTypeName (AST.TypeHeader'P4Type h) = h ^. #name
getTypeName _ = error "can't calculate name for type"

generatePacketOutEmit :: (Member (Writer [C.BlockItem]) r, CompC r) => AST.Expression -> [AST.Expression] -> Sem r (C.Type, C.Expr)
generatePacketOutEmit instance_ params = do
  let [param] = params
  let p4ty = gdrillField @"type_" param
  let name = "emit_packet_" <> getTypeName p4ty
  (ty, rawTy) <- generateP4Type p4ty
  body <- generateOutEmitBody rawTy
  packetStruct' <- simplifyType packetStruct
  modify . (<>) $
    defineFunc
      name
      (C.TypeSpec C.Void)
      [ C.Param (C.Ptr . C.TypeSpec $ packetStruct') "pkt",
        C.Param (C.Ptr . C.TypeSpec $ ty) "value"
      ]
      body
  expr <-
    generateCall'
      (name, C.TypeSpec C.Void)
      [ (True, C.TypeSpec packetStruct', instance_),
        (True, C.TypeSpec ty, param)
      ]
  pure (C.TypeSpec C.Void, expr)

getExternType :: Text -> C.TypeSpec
getExternType name = externs ^?! ix name . #type_

generateExternCall :: (Member (Writer [C.BlockItem]) r, CompC r) => Text -> Text -> AST.Expression -> [AST.Expression] -> Sem r (C.Type, C.Expr)
generateExternCall name methodName instance_ params =
  let method = externs ^?! ix name . #methods . ix methodName
   in generate method instance_ params
