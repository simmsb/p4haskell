-- |
module P4Haskell.Compile.Codegen.Extern
    ( generateExternCall
    , getExternType
     ) where

import Data.Generics.Sum
import Data.Text.Lens (unpacked)
import Data.Unique
import qualified Generics.SOP as GS
import qualified Language.C99.Simple as C
import {-# SOURCE #-} P4Haskell.Compile.Codegen.Typegen
import {-# SOURCE #-} P4Haskell.Compile.Codegen.MethodCall
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

data ExternInfo = ExternInfo
  { name    :: Text
  , type_   :: C.Type
  , methods :: HashMap Text ExternMethod
  }
  deriving ( Generic )

data ExternMethod = ExternMethod
  { name     :: Text
  , generate :: forall r. (Member (Writer [C.Stmt]) r, CompC r)
             => AST.Expression -> [AST.Expression] -> Sem r (C.Type, C.Expr)
  }

uint32_t :: C.Type
uint32_t = C.TypeSpec (C.TypedefName "uint32_t")

packetStruct :: C.Type
packetStruct = C.TypeSpec $ C.StructDecln (Just "packet") (fromList [ C.FieldDecln uint32_t "idx"
                                                                    , C.FieldDecln (C.Ptr . C.TypeSpec $ C.Char) "pkt"
                                                                    ])

externs :: HashMap Text ExternInfo
externs = fromList . map (toFst (^. #name)) $
  [ ExternInfo "packet_out" packetStruct packetOutMethods
  , ExternInfo "packet_in" packetStruct packetInMethods
  ]

r :: (r -> a) -> r -> a
r f = f

packetOutMethods :: HashMap Text ExternMethod
packetOutMethods = fromList . map (toFst $ r @ExternMethod name) $
  [ ExternMethod "emit" generatePacketOutEmit
  ]

packetInMethods :: HashMap Text ExternMethod
packetInMethods = fromList . map (toFst $ r @ExternMethod name) $
  [ ExternMethod "extract" generatePacketInExtract
  , ExternMethod "lookahead" generatePacketInLookahead
  , ExternMethod "advance" generatePacketInAdvance
  , ExternMethod "length" generatePacketInLength
  ]

generatePacketInExtract :: (Member (Writer [C.Stmt]) r, CompC r) => AST.Expression -> [AST.Expression] -> Sem r (C.Type, C.Expr)
generatePacketInExtract instance_ params = do
  undefined

generatePacketInLookahead :: (Member (Writer [C.Stmt]) r, CompC r) => AST.Expression -> [AST.Expression] -> Sem r (C.Type, C.Expr)
generatePacketInLookahead instance_ params = do
  undefined

generatePacketInAdvance :: (Member (Writer [C.Stmt]) r, CompC r) => AST.Expression -> [AST.Expression] -> Sem r (C.Type, C.Expr)
generatePacketInAdvance instance_ params = do
  undefined

generatePacketInLength :: (Member (Writer [C.Stmt]) r, CompC r) => AST.Expression -> [AST.Expression] -> Sem r (C.Type, C.Expr)
generatePacketInLength instance_ params = do
  undefined

findFields :: C.TypeSpec -> [(C.Expr -> C.Expr, Integer)]
findFields (C.StructDecln _ fields) = concatMap processField $ toList fields
  where processField (C.FieldDecln (C.TypeSpec ty) ident) = map (updateField ident) (findFields ty)
        updateField name (access, size) = ((\e -> C.Dot (access e) name), size)
findFields (C.TypedefName "uint8_t") = [(id, 1)]
findFields (C.TypedefName "uint16_t") = [(id, 2)]
findFields (C.TypedefName "uint32_t") = [(id, 3)]
findFields (C.TypedefName "uint64_t") = [(id, 4)]
findFields (C.TypedefName "int8_t") = [(id, 1)]
findFields (C.TypedefName "int16_t") = [(id, 2)]
findFields (C.TypedefName "int32_t") = [(id, 3)]
findFields (C.TypedefName "int64_t") = [(id, 4)]
findFields _ = error "unknown type for findFields"

generateOutEmitBody :: C.TypeSpec -> [C.Stmt]
generateOutEmitBody ty = concatMap generateWrite $ findFields ty
  where generateWrite (acc, size) = [ C.Expr $ C.Funcall (C.Ident "memcpy")
                                      [ (C.ref $ C.Arrow (C.Ident "pkt") "pkt") C..+ C.Arrow (C.Ident "pkt") "idx"
                                      , C.ref . acc . C.deref $ C.Ident "value"
                                      , C.LitInt size
                                      ]
                                    , C.Expr (C.Arrow (C.Ident "pkt") "idx" C..+= C.LitInt size)
                                    ]


getTypeName :: AST.P4Type -> Text
getTypeName (AST.TypeHeader'P4Type h) = h ^. #name
getTypeName _ = error "can't calculate name for type"

generatePacketOutEmit :: (Member (Writer [C.Stmt]) r, CompC r) => AST.Expression -> [AST.Expression] -> Sem r (C.Type, C.Expr)
generatePacketOutEmit instance_ params = do
  let [param] = params
  let p4ty = gdrillField @"type_" param
  let name = "emit_packet_" <> getTypeName p4ty
  (ty, rawTy) <- generateP4Type p4ty
  let C.TypeSpec rawTy' = rawTy
  tell $ defineFunc name (C.TypeSpec C.Void)
                          [ C.Param (C.Ptr . C.TypeSpec . C.TypedefName $ "uint32_t") "pkt"
                          , C.Param (C.Ptr ty) "value"
                          ]
                          (generateOutEmitBody rawTy')
  expr <- generateCall' (name, C.TypeSpec C.Void) [ (True, packetStruct, instance_)
                                                  , (True, ty, param)
                                                  ]
  pure (C.TypeSpec C.Void, expr)
 
getExternType :: Text -> C.Type
getExternType name = externs ^?! ix name . #type_

generateExternCall :: (Member (Writer [C.Stmt]) r, CompC r) => Text -> Text -> AST.Expression -> [AST.Expression] -> Sem r (C.Type, C.Expr)
generateExternCall name methodName instance_ params =
  let method = externs ^?! ix name . #methods . ix methodName
  in generate method instance_ params
