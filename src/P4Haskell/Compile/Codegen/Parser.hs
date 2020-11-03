-- |
module P4Haskell.Compile.Codegen.Parser
  ( generateParserStates,
  )
where

import Data.Generics.Sum
import Data.Text.Lens (unpacked)
import Data.Unique
import qualified Generics.SOP as GS
import qualified Language.C99.Simple as C
import P4Haskell.Compile.Codegen.Action
import P4Haskell.Compile.Codegen.Expression
import P4Haskell.Compile.Codegen.Extern
import {-# SOURCE #-} P4Haskell.Compile.Codegen.MethodCall
import P4Haskell.Compile.Codegen.Statement
import {-# SOURCE #-} P4Haskell.Compile.Codegen.Tables
import P4Haskell.Compile.Codegen.Typegen
import P4Haskell.Compile.Codegen.Utils
import P4Haskell.Compile.Eff
import P4Haskell.Compile.Scope
import qualified P4Haskell.Types.AST as AST
import P4Haskell.Utils.Drill
import Polysemy
import Polysemy.Fresh
import Polysemy.Reader
import Polysemy.Writer
import Relude (error)
import Relude.Extra (keys)

generateParserStates :: CompC r => Text -> AST.MapVec Text AST.ParserState -> Sem r [C.BlockItem]
generateParserStates n s = do
  si <- generateStateEnum n s
  stateVarName <- generateTempVar
  let stateVar = C.Ident stateVarName
  local (setParserStateInfoInScope si) $ undefined
  undefined

generateStateEnum :: CompC r => Text -> AST.MapVec Text AST.ParserState -> Sem r ParserStateInfo
generateStateEnum parserName s =
  let states = ["accept", "reject"] <> keys (s ^. #map)
      nameGen n = "parser_state_" <> parserName <> "_" <> n
      enumName = "parser_states_" <> parserName
      enumVariants = map (toString . nameGen) states
      stateMap = fromList [(n, C.Ident . toString . nameGen $ n) | n <- states]
      stateEnum = C.EnumDecln (Just . toString $ enumName) (fromList enumVariants)
   in do
        stateId <- hashUnique <$> fresh
        ty <- simplifyType stateEnum
        pure $ ParserStateInfo stateId ty stateMap

generateParserState :: CompC r => C.Expr -> Text -> AST.ParserState -> Sem r [C.BlockItem]
generateParserState stateVar name pstate = do
  body <- generateStatements $ pstate ^. #components
  selectStmts <- concat <$> traverse (generateSelectExpr stateVar) (pstate ^. #selectExpression)
  pure
    [ C.Stmt $ C.Label (toString name) $ C.Block (body <> selectStmts)
    ]

generateSelectExpr :: CompC r => C.Expr -> AST.ParserSelect -> Sem r [C.BlockItem]
generateSelectExpr stateVar ps = do
  (deps, expr) <- runWriterAssocR $ inner ps
  let stateWrite = [C.Stmt . C.Expr $ C.AssignOp C.Assign stateVar expr]
  pure (deps <> stateWrite)
  where
    inner (AST.SelectExpression'ParserSelect se) = generateP4Expression $ _Typed @AST.SelectExpression # se
    inner (AST.PathExpression'ParserSelect pe) = generateP4Expression $ _Typed @AST.PathExpression # pe
