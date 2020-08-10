-- | Things that are declared while compiling
module P4Haskell.Compile.Declared
  ( Declared (..),
    declareType,
    declareFunc,
  )
where

import Data.Generics.Labels ()
import Data.Monoid.Generic

data Declared = Declared
  { declaredTypes :: HashMap Text (),
    declaredFuncs :: HashMap Text ()
  }
  deriving (Show, Generic)
  deriving (Semigroup) via GenericSemigroup Declared
  deriving (Monoid) via GenericMonoid Declared

declareType :: Text -> () -> Declared
declareType n v = mempty & #declaredTypes . at n ?~ v

declareFunc :: Text -> () -> Declared
declareFunc n v = mempty & #declaredFuncs . at n ?~ v
