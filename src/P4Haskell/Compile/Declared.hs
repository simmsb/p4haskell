-- | Things that are declared while compiling
module P4Haskell.Compile.Declared
  ( Declared (..),
    declareType,
    defineFunc,
  )
where

import Data.Monoid.Generic
import qualified Language.C99.Simple as C

data Declared = Declared
  { declaredTypes :: HashMap Text C.Decln,
    declaredFuncs :: HashMap Text C.FunDef
  }
  deriving (Generic)
  deriving (Semigroup) via GenericSemigroup Declared
  deriving (Monoid) via GenericMonoid Declared

declareType :: Text -> C.Decln -> Declared
declareType n v = mempty & #declaredTypes . at n ?~ v

defineFunc :: Text -> C.Type -> [C.Param] -> [C.Stmt] -> Declared
defineFunc n ty params body = mempty & #declaredFuncs . at n ?~ C.FunDef ty (toString n) params [] body
