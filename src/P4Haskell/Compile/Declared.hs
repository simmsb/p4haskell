-- | Things that are declared while compiling
module P4Haskell.Compile.Declared
  ( Declared (..),
    declareType,
    declareFunc,
  )
where

import Data.Monoid.Generic
import qualified Language.C99.Simple as C

data Declared = Declared
  { declaredTypes :: HashMap Text C.Decln,
    declaredFuncs :: HashMap Text C.Decln
  }
  deriving (Generic)
  deriving (Semigroup) via GenericSemigroup Declared
  deriving (Monoid) via GenericMonoid Declared

declareType :: Text -> C.Decln -> Declared
declareType n v = mempty & #declaredTypes . at n ?~ v

declareFunc :: Text -> C.Type -> [C.Param] -> Declared
declareFunc n ty params = mempty & #declaredFuncs . at n ?~ C.FunDecln Nothing ty (toString n) params
