-- | Queries
module P4Haskell.Compile.Query
  (
  )
where

import Data.GADT.Compare.TH (deriveGEq)
import Data.Hashable
import Data.Some
import qualified Rock
import qualified P4Haskell.Types.AST as AST

data Query a where
  GetDeclInstance :: Text -> Query AST.DeclarationInstance

deriving instance Show (Query a)

deriveGEq ''Query

instance Hashable (Query a) where
  hashWithSalt salt q =
    case q of
      GetDeclInstance k -> h 0 k
    where
      {-# INLINE h #-}
      h :: forall h. Hashable h => Int -> h -> Int
      h tag payload =
        hashWithSalt salt (tag, payload)

instance Hashable (Some Query) where
  {-# INLINE hash #-}
  hash (Some query) =
    hash query

  {-# INLINE hashWithSalt #-}
  hashWithSalt salt (Some query) =
    hashWithSalt salt query
