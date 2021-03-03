-- |
module P4Haskell.Compile.Codegen.Utils (
  generateTempVar,
  removeDeadExprs,
  fromJustNote,
  getDevFnAttrs,
  getConstAttrs,
  getGlobalFnAttrs,
) where

import Data.Unique
import qualified Language.C99.Simple as C
import P4Haskell.Compile.Eff
import P4Haskell.Compile.Opts
import qualified Polysemy as P
import qualified Polysemy.Fresh as P
import qualified Polysemy.Reader as P
import Relude

generateTempVar :: P.Member (P.Fresh Unique) r => P.Sem r C.Ident
generateTempVar = do
  i <- hashUnique <$> P.fresh
  pure $ "tmp_var_" <> show i

-- HACK: we use (LitInt 0) to signal void expressions
removeDeadExprs :: [C.BlockItem] -> [C.BlockItem]
removeDeadExprs = filter notDeadExpr
 where
  notDeadExpr (C.Stmt (C.Expr (C.LitInt _))) = False
  notDeadExpr _ = True

fromJustNote :: Text -> Maybe a -> a
fromJustNote _ (Just a) = a
fromJustNote msg _ = error msg

getDevFnAttrs :: CompC r => P.Sem r (Maybe Text)
getDevFnAttrs = do
  Opts{cpuMode} <- P.ask
  pure $ if cpuMode then Just "static" else Just "__device__ static"

getConstAttrs :: CompC r => P.Sem r (Maybe Text)
getConstAttrs = do
  Opts{cpuMode} <- P.ask
  pure $ if cpuMode then Nothing else Just "__constant__"

getGlobalFnAttrs :: CompC r => P.Sem r (Maybe Text)
getGlobalFnAttrs = do
  Opts{cpuMode} <- P.ask
  pure $ if cpuMode then Nothing else Just "extern \"C\" __global__"
