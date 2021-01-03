{- | A list and a map in one, stuff like ordered maps don't have nice interop so
 I'll use this
-}
module P4Haskell.Types.AST.MapVec (
  MapVec (..),
) where

import Control.Lens
import Data.Generics.Labels ()
import Relude
import Text.Show

data MapVec k v = MapVec
  { map :: HashMap k v
  , vec :: [v]
  }
  deriving stock (Generic, Eq)
  deriving anyclass (Hashable)

instance Show v => Show (MapVec k v) where
  showsPrec i (MapVec _ vec) = showsPrec i vec

instance Foldable (MapVec k) where
  foldMap f m = foldMap f (m ^. #vec)
  foldr f i m = foldr f i (m ^. #vec)

instance Functor (MapVec k) where
  fmap f (MapVec m v) = MapVec (fmap f m) (fmap f v)
