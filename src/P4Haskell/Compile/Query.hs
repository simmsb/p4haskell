-- | Queries
module P4Haskell.Compile.Query
  ( Query (..),
  )
where

import Data.GADT.Compare.TH (deriveGEq)
import Data.Hashable
import Data.Some
import qualified P4Haskell.Types.AST as AST
import qualified Language.C99.Simple as C

data Query a where
  GetMain        :: Query AST.DeclarationInstance
  FetchType      :: Text -> Query AST.TopLevelTypeDecl
  GenerateP4Type :: AST.P4Type -> Query (C.TypeSpec, [(Text, C.TypeSpec)])

deriving instance Show (Query a)

deriveGEq ''Query

instance Hashable (Query a) where
  hashWithSalt salt q =
    case q of
      GetMain -> h 0 ()
      FetchType t -> h 1 t
      GenerateP4Type t -> h 2 t
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
