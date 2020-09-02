{-# LANGUAGE QuasiQuotes #-}

-- |
module P4Haskell.Compile.Codegen.Extern
  ( generateExternCall,
    getExternType,
  )
where

import Control.Monad.Extra (concatMapM)
import qualified Language.C99.Simple as C
import {-# SOURCE #-} P4Haskell.Compile.Codegen.MethodCall
import {-# SOURCE #-} P4Haskell.Compile.Codegen.Typegen
import P4Haskell.Compile.Codegen.Utils
import P4Haskell.Compile.Declared
import P4Haskell.Compile.Eff
import qualified P4Haskell.Types.AST as AST
import P4Haskell.Utils.Drill
import Polysemy
import Polysemy.State
import Polysemy.Writer
import PyF
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

uint8_t :: C.TypeSpec
uint8_t = C.TypedefName "uint8_t"

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

defineWritePartial :: CompC r => Sem r ()
defineWritePartial =
  modify . (<>) $
    defineFunc
      "write_partial"
      (C.TypeSpec C.Void)
      [ C.Param (C.Ptr $ C.TypeSpec uint8_t) "addr",
        C.Param (C.TypeSpec uint8_t) "width",
        C.Param (C.TypeSpec uint8_t) "shift",
        C.Param (C.TypeSpec uint8_t) "value"
      ]
      [ C.Stmt . C.Expr $ (addr C..= (addr C..& (C..~) (mask width C..<< shift)) C..| (value C..<< shift))
      ]
  where
    addr = C.deref $ C.Ident "addr"
    width = C.Ident "width"
    shift = C.Ident "shift"
    value = C.Ident "value"
    uint8_t1 = C.Cast (C.TypeName $ C.TypeSpec uint8_t) (C.LitInt 1)
    mask n = (uint8_t1 C..<< n) C..- uint8_t1

findFields :: CompC r => AST.P4Type -> Sem r [(C.Expr -> C.Expr, Int)]
findFields (AST.TypeHeader'P4Type (AST.TypeHeader _ _ fields)) = (concat <$>) . mapM processField $ fields ^. #vec
  where
    processField :: CompC r => AST.StructField -> Sem r [(C.Expr -> C.Expr, Int)]
    processField (AST.StructField ident _ ty) = do
      ty' <- resolveP4Type ty
      fields' <- findFields ty'
      pure $ map (updateField ident) fields'
    updateField name (access, size) = ((\e -> access $ C.Dot e (toString name)), size)
findFields (AST.TypeBits'P4Type (AST.TypeBits s _)) = pure [(id, s)]
findFields t = error $ "unknown type for findFields: " <> show t

generateOutEmitBody :: forall r. CompC r => AST.P4Type -> Sem r [C.BlockItem]
generateOutEmitBody ty = do
  defineWritePartial
  (evalState @Int 0 . concatMapM generateWrite) =<< findFields ty
  where
    sizes =
      [ (8, Nothing),
        (16, Just "htons"),
        (32, Just "htonl"),
        (64, Just "htonll")
      ]
    generateWrite :: (C.Expr -> C.Expr, Int) -> Sem (State Int ': r) [C.BlockItem]
    generateWrite (acc, size) = do
      align <- get @Int
      Polysemy.State.put $ (align + size) `mod` 8
      generateWrite' (acc, size) align
    generateWrite' :: (C.Expr -> C.Expr, Int) -> Int -> Sem (State Int ': r) [C.BlockItem]
    generateWrite' (acc, size) align =
      let (loadSize, swapFn) = case find ((size <=) . fst) sizes of
            Just ls -> ls
            Nothing -> error $ "unsupported bit width: " <> show size
          numBytes = (size + 7) `div` 8
          shiftAmt =
            fromIntegral $
              if size < 8
                then loadSize - align - size
                else loadSize - size
          val = acc . C.deref $ C.Ident "value"
          expr = case swapFn of
            Just fn -> C.Funcall (C.Ident fn) [val C..<< C.LitInt shiftAmt]
            Nothing -> val
          tmpType = C.TypeSpec . C.TypedefName $ "uint" <> show loadSize <> "_t"
       in do
            tmpName <- generateTempVar
            tmpRefName <- generateTempVar
            let declns =
                  [ C.Decln $ C.VarDecln Nothing tmpType tmpName (Just . C.InitExpr $ expr),
                    C.Decln $ C.VarDecln Nothing (C.Ptr . C.TypeSpec $ C.Char) tmpRefName (Just . C.InitExpr . C.ref $ tmpVar)
                  ]
                tmpVar = C.Ident tmpName
                tmpRefVar = C.Ident tmpRefName
                writes = generateIndividualWrites (fromIntegral size) (fromIntegral numBytes) shiftAmt tmpRefVar (fromIntegral align)
                after = [C.Stmt $ C.Expr (C.Arrow (C.Ident "pkt") "idx" C..+= C.LitInt (fromIntegral size))]
             in pure $ declns <> writes <> after

    loadPacketWOffset i = C.Arrow (C.Ident "pkt") "pkt" C..+ (C.Arrow (C.Ident "pkt") "idx" C..* C.LitInt 8) C..+ C.LitInt i
    generateIndividualWrites size numBytes shiftAmt tmpRefVar align = go 0 shiftAmt size align
      where
        go i shift left align
          | i < numBytes =
            let val = C.Index tmpRefVar (C.LitInt i)
                freeBits = if align /= 0 then 8 - align else 8
                bitsInCurrentByte' = min 8 left
                toWrite = if bitsInCurrentByte' > freeBits then freeBits else bitsInCurrentByte'
                (firstWrite, shift') =
                  if align == 0 && toWrite == 8
                    then (C.Stmt . C.Expr $ (loadPacketWOffset i C..= val), shift)
                    else
                      let shift'' = 8 - align - toWrite
                          rshift = case (size > freeBits, align == 0) of
                            (True, True) -> shift''
                            (True, False) -> align
                            (False, _) -> 0
                       in ( C.Stmt . C.Expr $
                              C.Funcall
                                (C.Ident "write_partial")
                                [C.ref (loadPacketWOffset i), C.LitInt toWrite, C.LitInt shift'', val C..>> C.LitInt rshift],
                            shift''
                          )
                left' = left - toWrite
                bitsInCurrentByte'' = bitsInCurrentByte' - toWrite
                align' = (align + toWrite) `mod` 8
                toWrite' = 8 - toWrite
                (secondWrite, left'') =
                  if bitsInCurrentByte'' > 0
                    then
                      let write =
                            if toWrite' == 8
                              then [C.Stmt . C.Expr $ ((C.ref (loadPacketWOffset i) C..+ C.LitInt 1) C..= (val C..<< C.LitInt (8 - align' `mod` 8)))]
                              else
                                [ C.Stmt . C.Expr $
                                    C.Funcall
                                      (C.Ident "write_partial")
                                      [C.ref (loadPacketWOffset i) C..+ C.LitInt 1, C.LitInt toWrite', C.LitInt (8 + align' - toWrite'), val]
                                ]
                       in (write, left' - toWrite')
                    else ([], left')
             in (firstWrite : secondWrite) <> go (i + 1) shift' left'' align'
        go _ _ _ _ = []

getTypeName :: AST.P4Type -> Text
getTypeName (AST.TypeHeader'P4Type h) = h ^. #name
getTypeName _ = error "can't calculate name for type"

generatePacketOutEmit :: (Member (Writer [C.BlockItem]) r, CompC r) => AST.Expression -> [AST.Expression] -> Sem r (C.Type, C.Expr)
generatePacketOutEmit instance_ params = do
  let [param] = params
  let p4ty = gdrillField @"type_" param
  let name = "emit_packet_" <> getTypeName p4ty
  (ty, _rawTy) <- generateP4Type p4ty
  body <- generateOutEmitBody =<< resolveP4Type p4ty
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
