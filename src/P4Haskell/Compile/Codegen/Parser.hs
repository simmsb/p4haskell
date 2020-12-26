-- |
module P4Haskell.Compile.Codegen.Parser
  ( generateParserStates,
  )
where

import Control.Lens
import Data.Generics.Sum
import Data.Unique
import qualified Language.C99.Simple as C
import P4Haskell.Compile.Codegen.Expression
import P4Haskell.Compile.Codegen.Statement
import P4Haskell.Compile.Codegen.Typegen
import P4Haskell.Compile.Codegen.Utils
import P4Haskell.Compile.Eff
import P4Haskell.Compile.Scope
import qualified P4Haskell.Types.AST as AST
import qualified Polysemy as P
import qualified Polysemy.Fresh as P
import qualified Polysemy.Reader as P
import qualified Polysemy.Writer as P
import Relude
import Relude.Extra (keys, toPairs)

generateParserStates :: CompC r => Text -> AST.MapVec Text AST.ParserState -> P.Sem r [C.BlockItem]
generateParserStates n s = do
  stateVarName <- generateTempVar
  let stateVar = C.Ident stateVarName
  si <- generateStateEnum n stateVar s
  let stateVarStart = si ^?! #states . ix "start"
  let stateVarInit = [C.Decln $ C.VarDecln Nothing (C.TypeSpec $ si ^. #enumTy) stateVarName (Just . C.InitExpr $ stateVarStart)]
  let stateMap = s ^. #map & sans "accept" & sans "reject"
  cases <- P.local (setParserStateInfoInScope si) $ forM (toPairs stateMap) \(sn, ps) -> do
    body <- generateParserState stateVar ps
    let stateEnumVariant = si ^?! #states . ix sn
    pure $ C.Case stateEnumVariant body
  let extraCases =
        [ C.Case (si ^?! #states . ix "accept") $ C.Block [C.Stmt $ C.Return (Just $ C.LitBool True)],
          C.Case (si ^?! #states . ix "reject") $ C.Block [C.Stmt $ C.Return (Just $ C.LitBool False)]
        ]
  pure (stateVarInit <> [C.Stmt $ C.ForInf [C.Stmt $ C.Switch stateVar (cases <> extraCases)]])

generateStateEnum :: CompC r => Text -> C.Expr -> AST.MapVec Text AST.ParserState -> P.Sem r ParserStateInfo
generateStateEnum parserName stateVar s =
  let states = keys (s ^. #map)
      nameGen n = "parser_state_" <> parserName <> "_" <> n
      enumName = "parser_states_" <> parserName
      enumVariants = map (toString . nameGen) states
      stateMap = fromList [(n, C.Ident . toString . nameGen $ n) | n <- states]
      stateEnum = C.EnumDecln (Just . toString $ enumName) (fromList enumVariants)
   in do
        stateId <- hashUnique <$> P.fresh
        ty <- simplifyType stateEnum
        pure $ ParserStateInfo stateId stateVar ty stateMap

generateParserState :: CompC r => C.Expr -> AST.ParserState -> P.Sem r C.Stmt
generateParserState stateVar pstate = do
  body <- generateStatements $ pstate ^. #components
  selectStmts <- concat <$> traverse (generateSelectExpr stateVar) (pstate ^. #selectExpression)
  pure $ C.Block (body <> selectStmts)

generateSelectExpr :: CompC r => C.Expr -> AST.ParserSelect -> P.Sem r [C.BlockItem]
generateSelectExpr stateVar ps = do
  (deps, expr) <- P.runWriterAssocR $ inner ps
  let stateWrite = [C.Stmt . C.Expr $ C.AssignOp C.Assign stateVar expr]
  pure (deps <> stateWrite <> [C.Stmt C.Break])
  where
    inner (AST.SelectExpression'ParserSelect se) = generateP4Expression $ _Typed @AST.SelectExpression # se
    inner (AST.PathExpression'ParserSelect pe) = generateP4Expression $ _Typed @AST.PathExpression # pe
