-- |
module P4Haskell.Compile.Codegen.Extern
  ( generateExternCall,
    getExternType,
    packetStruct,
  )
where

import Control.Lens
import Control.Monad.Extra (concatMapM)
import Data.List.Extra (groupOn)
import qualified Language.C99.Simple as C
import {-# SOURCE #-} P4Haskell.Compile.Codegen.MethodCall
import {-# SOURCE #-} P4Haskell.Compile.Codegen.Typegen
import P4Haskell.Compile.Codegen.Utils
import P4Haskell.Compile.Declared
import P4Haskell.Compile.Eff
import P4Haskell.Compile.Scope
import qualified P4Haskell.Types.AST as AST
import P4Haskell.Utils.Drill
import qualified Polysemy as P
import qualified Polysemy.Membership as P
import qualified Polysemy.State as P
import qualified Polysemy.Tagged as P
import qualified Polysemy.Writer as P
import Relude
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
      (P.Member (P.Writer [C.BlockItem]) r, CompC r) =>
      AST.Expression ->
      [AST.Expression] ->
      P.Sem r (C.Type, C.Expr)
  }

uint8_t :: C.TypeSpec
uint8_t = C.TypedefName "uint8_t"

uint64_t :: C.TypeSpec
uint64_t = C.TypedefName "uint64_t"

uint16_t :: C.TypeSpec
uint16_t = C.TypedefName "uint16_t"

uint32_t :: C.TypeSpec
uint32_t = C.TypedefName "uint32_t"

packetStruct :: C.TypeSpec
packetStruct =
  C.StructDecln
    (Just "packet")
    ( fromList
        [ C.FieldDecln (C.TypeSpec uint16_t) "offset",
          C.FieldDecln (C.TypeSpec uint16_t) "end",
          C.FieldDecln (C.TypeSpec uint16_t) "base",
          C.FieldDecln (C.Ptr . C.TypeSpec $ C.Char) "pkt"
        ]
    )

ptrPacketStruct :: C.TypeSpec
ptrPacketStruct =
  C.StructDecln
    (Just "ppacket")
    ( fromList
        [ C.FieldDecln (C.Ptr . C.TypeSpec $ packetStruct) "ppkt"
        ]
    )

externs :: HashMap Text ExternInfo
externs =
  fromList . map (toFst (^. #name)) $
    [ ExternInfo "packet_out" ptrPacketStruct packetOutMethods,
      ExternInfo "packet_in" ptrPacketStruct packetInMethods
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

generatePacketInExtract :: (P.Member (P.Writer [C.BlockItem]) r, CompC r) => AST.Expression -> [AST.Expression] -> P.Sem r (C.Type, C.Expr)
generatePacketInExtract instance_ [param] = do
  let p4ty = gdrillField @"type_" param
  let name = "extract_packet_" <> getTypeName p4ty
  (ty, _rawTy) <- generateP4Type p4ty

  case P.tryMembership @(P.Tagged "packet-size" (P.Writer (Sum Int))) of
    Just pr ->
      P.subsumeUsing pr $ do
        outSize <- sum . map snd <$> findFields p4ty
        P.tagged @"packet-size" $ P.tell $ Sum outSize
    _ -> pure ()

  body <- generateInExtractBody (C.Arrow (C.Ident "ppkt") "ppkt") (C.Ident "hdr") =<< resolveP4Type p4ty
  packetStruct' <- simplifyType packetStruct
  ptrPacketStruct' <- simplifyType ptrPacketStruct
  P.modify . (<>) $
    defineFunc
      name
      (C.TypeSpec C.Bool)
      [ C.Param (C.Ptr . C.TypeSpec $ ptrPacketStruct') "ppkt",
        C.Param (C.Ptr . C.TypeSpec $ ty) "hdr"
      ]
      body

  expr <-
    generateCall'
      (name, C.TypeSpec C.Bool)
      [ (False, C.TypeSpec packetStruct', instance_),
        (True, C.TypeSpec ty, param)
      ]

  stateEnumInfo <- fromJustNote "stateEnumInfo" <$> fetchParserStateInfoInScope
  let stateVar = stateEnumInfo ^. #stateVar
  let reject = stateEnumInfo ^?! #states . ix "reject"

  P.tell
    [ C.Stmt $
        C.If
          (C.UnaryOp C.BoolNot expr)
          [ C.Stmt . C.Expr $ C.AssignOp C.Assign stateVar reject,
            C.Stmt C.Break
          ]
    ]
  pure (C.TypeSpec C.Void, C.LitInt 0)

-- | map bytes to fields of the header
-- returns list of tuples of [[(accessor, (read start, read end), (write start, write end), field size)])]
-- where each consecutive list is the reads from each consecutive byte
mapBytesToFields :: [(C.Expr -> C.Expr, Int)] -> [[(C.Expr -> C.Expr, (Int, Int), (Int, Int), Int)]]
mapBytesToFields = map (map snd) . groupOn fst . go 0 0 0
  where
    go :: Int -> Int -> Int -> [(C.Expr -> C.Expr, Int)] -> [(Int, (C.Expr -> C.Expr, (Int, Int), (Int, Int), Int))]
    go byteIndex byteOffset fieldOffset l@((f, width) : xs) =
      let remaining = width - fieldOffset
          (byteIndex', byteOffset', readEnd) =
            if (remaining + byteOffset) > 8
              then (byteIndex + 1, 0, 8)
              else (byteIndex, remaining + byteOffset, remaining + byteOffset)
          writeEnd = fieldOffset + (readEnd - byteOffset)
          fieldOffset' = if writeEnd == width then 0 else writeEnd
          e = (byteIndex, (f, (byteOffset, readEnd), (fieldOffset, writeEnd), width))
       in if fieldOffset' == 0
            then e : go byteIndex' byteOffset' fieldOffset' xs
            else e : go byteIndex' byteOffset' fieldOffset' l
    go _ _ _ _ = []

generateExtractionForByte :: CompC r => C.Expr -> C.Expr -> Int -> [(C.Expr -> C.Expr, (Int, Int), (Int, Int), Int)] -> P.Sem r [C.BlockItem]
generateExtractionForByte packetBuf targetHdr byteIndex pieces = do
  let byte = C.Index packetBuf (C.LitInt $ fromIntegral byteIndex)
  tmpName <- generateTempVar
  let declns = [C.Decln $ C.VarDecln Nothing (C.TypeSpec C.Char) tmpName (Just . C.InitExpr $ byte)]
  let tmp = C.Ident tmpName
  let piecesStmts = map (generatePieceLoad tmp targetHdr) pieces
  pure (declns <> piecesStmts)
  where
    uint64_t1 = C.Cast (C.TypeName $ C.TypeSpec uint64_t) (C.LitInt 1)
    mask n = (uint64_t1 C..<< C.LitInt n) C..- uint64_t1
    generatePieceLoad
      byte
      hdr
      ( accessor,
        (fromIntegral -> readStart, fromIntegral -> readEnd),
        (fromIntegral -> writeStart, fromIntegral -> writeEnd),
        fromIntegral -> size
        ) =
        let readShift = C.LitInt $ 8 - readEnd
            readMask = mask $ readEnd - readStart
            writeShift = C.LitInt $ size - writeEnd
            writeMask = (C..~) (mask (writeEnd - writeStart) C..<< writeShift)
            read = (byte C..>> readShift) C..& readMask
            writeExpr = (accessor hdr C..& writeMask) C..| (read C..<< writeShift)
         in C.Stmt . C.Expr $ accessor hdr C..= writeExpr

generatePostProcessExtract :: C.Expr -> [(C.Expr -> C.Expr, Int)] -> [C.BlockItem]
generatePostProcessExtract targetExpr = concatMap inner
  where
    inner (accessor, width) =
      let swapFn = case find ((width <=) . fst) sizes of
            Just (_, fn) -> fn
            Nothing -> error $ "unsupported byte width: " <> show width
       in case swapFn of
            Just fn -> [C.Stmt . C.Expr $ accessor targetExpr C..= C.Funcall (C.Ident fn) [accessor targetExpr]]
            Nothing -> []
    sizes =
      [ (8, Nothing),
        (16, Just "htons"),
        (32, Just "htonl"),
        (64, Just "htonll")
      ]

generateInExtractBody :: forall r. CompC r => C.Expr -> C.Expr -> AST.P4Type -> P.Sem r [C.BlockItem]
generateInExtractBody packet targetHdr ty = do
  fields <- findFields ty
  let packetSize = fromIntegral . sum $ map snd fields
  let mapped = mapBytesToFields fields
  let doExtract = uncurry $ generateExtractionForByte (C.Arrow packet "pkt") (C.deref targetHdr)
  let checkStep =
        [ C.Stmt $
            C.If
              (C.Arrow packet "end" C..< (C.Arrow packet "offset" C..+ C.LitInt packetSize))
              [ C.Stmt . C.Return . Just . C.LitBool $ False
              ]
        ]
  extractStep <- concat <$> forM (zip [0 ..] mapped) doExtract
  let postProcessStep = generatePostProcessExtract (C.deref targetHdr) fields
  let lastStep =
        [ C.Stmt . C.Expr $ C.AssignOp C.AssignAdd (C.Arrow packet "offset") (C.LitInt packetSize),
          C.Stmt . C.Expr $ C.AssignOp C.Assign (C.Arrow targetHdr "valid") (C.LitBool True),
          C.Stmt . C.Return . Just . C.LitBool $ True
        ]
  pure (checkStep <> extractStep <> postProcessStep <> lastStep)

--      0      1       2       3       4       5       6
-- [--------++++++++--------++++++++--------++++++++--------]
-- [---++++++---------------------------------++------------]
--   a   b                  c                 d     e
--
-- load 0, store first 3 bits into a[0..3]
--         store last 5 bits into b[1..6]
-- load 1, store first bit into b[0..1]
--         store last 7 bits into c[26..33]
-- load 2, store into c[18..33]
-- etc
-- so, [(a, 0, (0..3), (0..3),   3)
--     ,(b, 0, (3..8), (0..5),   6)
--     ,(b, 1, (0..1), (5..6),   6)
--     ,(c, 1, (1..8), (0..7),   33)
--     ,(c, 2, (0..8), (7..15),  33)
--     ,(c, 3, (0..8), (15..23), 33)
--     ,(c, 4, (0..8), (23..31), 33)
--     ,(c, 5, (0..2), (31..33), 33)
--     ,(d, 5, (2..4), (0..2),   2)
--     ]

generatePacketInLookahead :: (P.Member (P.Writer [C.BlockItem]) r, CompC r) => AST.Expression -> [AST.Expression] -> P.Sem r (C.Type, C.Expr)
generatePacketInLookahead instance_ params = do
  error "packetlookahead"

generatePacketInAdvance :: (P.Member (P.Writer [C.BlockItem]) r, CompC r) => AST.Expression -> [AST.Expression] -> P.Sem r (C.Type, C.Expr)
generatePacketInAdvance instance_ params = do
  error "packetadvance"

generatePacketInLength :: (P.Member (P.Writer [C.BlockItem]) r, CompC r) => AST.Expression -> [AST.Expression] -> P.Sem r (C.Type, C.Expr)
generatePacketInLength instance_ params = do
  error "packetlength"

defineWritePartial :: CompC r => P.Sem r ()
defineWritePartial =
  P.modify . (<>) $
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

-- | generate a list of (attribute function, width) for a p4 header
--
-- where 'attribute function' is a function that given an expression accesses
-- the C struct field corresponding to the p4 header field
findFields :: CompC r => AST.P4Type -> P.Sem r [(C.Expr -> C.Expr, Int)]
findFields (AST.TypeHeader'P4Type (AST.TypeHeader _ _ fields)) = (concat <$>) . mapM processField $ fields ^. #vec
  where
    processField :: CompC r => AST.StructField -> P.Sem r [(C.Expr -> C.Expr, Int)]
    processField (AST.StructField ident _ ty) = do
      ty' <- resolveP4Type ty
      fields' <- findFields ty'
      pure $ map (updateField ident) fields'
    updateField name (access, size) = (\e -> access $ C.Dot e (toString name), size)
findFields (AST.TypeBits'P4Type (AST.TypeBits s _)) = pure [(id, s)]
findFields t = error $ "unknown type for findFields: " <> show t

generateOutEmitBody :: forall r. CompC r => AST.P4Type -> P.Sem r [C.BlockItem]
generateOutEmitBody ty = do
  defineWritePartial
  (P.evalState @Int 0 . concatMapM generateWrite) =<< findFields ty
  where
    pkt = C.Arrow (C.Ident "ppkt") "ppkt"
    sizes =
      [ (8, Nothing),
        (16, Just "htons"),
        (32, Just "htonl"),
        (64, Just "htonll")
      ]
    generateWrite :: (C.Expr -> C.Expr, Int) -> P.Sem (P.State Int ': r) [C.BlockItem]
    generateWrite (acc, size) = do
      align <- P.get @Int
      P.put $ (align + size) `mod` 8
      generateWrite' (acc, size) align
    generateWrite' :: (C.Expr -> C.Expr, Int) -> Int -> P.Sem (P.State Int ': r) [C.BlockItem]
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
                after = [C.Stmt $ C.Expr (C.Arrow pkt "offset" C..+= C.LitInt (fromIntegral size))]
             in pure $ declns <> writes <> after

    loadPacketWOffset i = C.Arrow pkt "pkt" C..+ (C.Arrow pkt "offset" C..* C.LitInt 8) C..+ C.LitInt i
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

generatePacketOutEmit :: (P.Member (P.Writer [C.BlockItem]) r, CompC r) => AST.Expression -> [AST.Expression] -> P.Sem r (C.Type, C.Expr)
generatePacketOutEmit instance_ [param] = do
  let p4ty = gdrillField @"type_" param
  let name = "emit_packet_" <> getTypeName p4ty
  (ty, _rawTy) <- generateP4Type p4ty

  case P.tryMembership @(P.Tagged "packet-size" (P.Writer (Sum Int))) of
    Just pr ->
      P.subsumeUsing pr $ do
        outSize <- sum . map snd <$> findFields p4ty
        P.tagged @"packet-size" $ P.tell $ Sum outSize
    _ -> pure ()

  body <- generateOutEmitBody =<< resolveP4Type p4ty
  ptrPacketStruct' <- simplifyType ptrPacketStruct
  P.modify . (<>) $
    defineFunc
      name
      (C.TypeSpec C.Void)
      [ C.Param (C.TypeSpec ptrPacketStruct') "ppkt",
        C.Param (C.Ptr . C.TypeSpec $ ty) "value"
      ]
      body
  expr <-
    generateCall'
      (name, C.TypeSpec C.Void)
      [ (False, C.TypeSpec ptrPacketStruct', instance_),
        (True, C.TypeSpec ty, param)
      ]
  pure (C.TypeSpec C.Void, expr)

getExternType :: Text -> C.TypeSpec
getExternType name = externs ^?! ix name . #type_

generateExternCall :: (P.Member (P.Writer [C.BlockItem]) r, CompC r) => Text -> Text -> AST.Expression -> [AST.Expression] -> P.Sem r (C.Type, C.Expr)
generateExternCall name methodName instance_ params =
  let method = externs ^?! ix name . #methods . ix methodName
   in generate method instance_ params
