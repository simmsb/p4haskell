-- |
module P4Haskell.Compile.Codegen.Utils
    ( generateTempVar
     ) where

import Data.Unique
import qualified Language.C99.Simple as C
import Polysemy
import Polysemy.Fresh

generateTempVar :: Member (Fresh Unique) r => Sem r C.Ident
generateTempVar = do
  i <- hashUnique <$> fresh
  pure $ "tmp_var_" <> show i
