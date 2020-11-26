-- |
module P4Haskell.Compile.Codegen.Utils
    ( generateTempVar
    , removeDeadExprs
    , fromJustNote
     ) where

import Data.Unique
import qualified Language.C99.Simple as C
import Polysemy
import Polysemy.Fresh
import Relude.Debug (error)

generateTempVar :: Member (Fresh Unique) r => Sem r C.Ident
generateTempVar = do
  i <- hashUnique <$> fresh
  pure $ "tmp_var_" <> show i


-- HACK: we use (LitInt 0) to signal void expressions
removeDeadExprs :: [C.BlockItem] -> [C.BlockItem]
removeDeadExprs = filter notDeadExpr
  where notDeadExpr (C.Stmt (C.Expr (C.LitInt _))) = False
        notDeadExpr _ = True

fromJustNote :: Text -> Maybe a -> a
fromJustNote _ (Just a) = a
fromJustNote msg _ = error msg
