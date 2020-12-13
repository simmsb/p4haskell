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
import P4Haskell.Compile.Codegen.Extern
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
import qualified Polysemy.Tagged as P
import qualified Polysemy.Writer as P

generateMain :: CompC r => P.Sem r ()
generateMain = do
  main <- fetch GetMain

  let parseName = main ^. #arguments . ix 0 . #expression . #constructedType . _Typed @AST.TypeName . #path . #name
  let pipeName = main ^. #arguments . ix 1 . #expression . #constructedType . _Typed @AST.TypeName . #path . #name
  let dprsName = main ^. #arguments . ix 2 . #expression . #constructedType . _Typed @AST.TypeName . #path . #name

  parsers <- fetch GetTopLevelParser
  controls <- fetch GetTopLevelControl

  let prsAST = parsers ^?! ix parseName
  (prsParser, inPktSize) <- generateParser prsAST

  let pipeAST = controls ^?! ix pipeName
  pipeControl <- generateControl pipeAST

  let dprsAST = controls ^?! ix dprsName
  dprsControl <- generateDeparse inPktSize dprsAST

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

getPktSize :: P.Sem (P.Tagged "packet-size" (P.Writer (Sum Int)) ': r) a -> P.Sem r (Int, a)
getPktSize = ((_1 %~ getSum) <$>) . P.runWriter @(Sum Int) . P.untag @"packet-size"

generateParser :: CompC r => AST.P4Parser -> P.Sem r (C.Ident, Int)
generateParser p = do
  (params, vars) <- generateParams $ p ^. #type_ . #applyParams . #vec
  let scopeUpdate scope = flipfoldl' addVarToScope scope vars
  (inPktSize, body) <- getPktSize . P.local scopeUpdate $ generateParserStates (p ^. #name) (p ^. #states)
  let body' = removeDeadExprs body
  P.modify . (<>) $ defineFunc (p ^. #name) (C.TypeSpec C.Void) params body'
  pure (p ^. #name . unpacked, inPktSize)

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

generatePacketAdjust :: CompC r => Int -> Int -> C.Expr -> P.Sem r [C.BlockItem]
generatePacketAdjust inPktSize newPktSize pkt = do
  packetStruct' <- simplifyType packetStruct
  P.modify . (<>) $
    defineFunc
      "adjust_packet"
      (C.TypeSpec C.Void)
      [ C.Param (C.Ptr . C.TypeSpec $ packetStruct') "pkt",
        C.Param (C.TypeSpec C.Int) "current_size",
        C.Param (C.TypeSpec C.Int) "final_size"
      ]
      []
  -- TODO: body

  pure
    [ C.Stmt . C.Expr $
        C.Funcall
          (C.Ident "adjust_packet")
          [ pkt,
            C.LitInt $ fromIntegral inPktSize,
            C.LitInt $ fromIntegral newPktSize
          ]
    ]

generateDeparse :: forall r. CompC r => Int -> AST.P4Control -> P.Sem r C.Ident
generateDeparse inPktSize c = do
  (params, vars) <- generateParams $ c ^. #type_ . #applyParams . #vec
  let localVars = c ^.. #controlLocals . #vec . traverse . _Typed @AST.DeclarationVariable
  let actions = c ^.. #controlLocals . #vec . traverse . _Typed @AST.P4Action
  let scopeUpdate scope =
        let withVars = flipfoldl' addVarToScope scope vars
         in flipfoldl' addActionToScope withVars actions
  (outPacketSize, body) <-
    getPktSize
      . P.local scopeUpdate
      . generateStatements
      $ map injectTyped localVars <> (c ^. #body . #components)

  let pktVarName = toString $ vars ^?! ix 0 . #varOriginalName

  adjustBody <- generatePacketAdjust inPktSize outPacketSize (C.Ident pktVarName)

  let body' = adjustBody <> removeDeadExprs body
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
