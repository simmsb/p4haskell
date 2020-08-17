-- | A list and a map in one, stuff like ordered maps don't have nice interop so
-- I'll use this
module P4Haskell.Types.AST.MapVec
    ( MapVec(..)
    ) where

import Text.Show

data MapVec k v = MapVec
  { map :: HashMap k v
  , vec :: [v]
  }
  deriving ( Generic, Eq, Hashable )

instance Show v => Show (MapVec k v) where
  showsPrec i (MapVec _ vec) = showsPrec i vec
