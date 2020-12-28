-- | Queries
module P4Haskell.Compile.Query
  ( Query (..),
  )
where

import Data.GADT.Compare.TH (deriveGEq)
import Data.Hashable
import Data.Some
import qualified Language.C99.Simple as C
import qualified P4Haskell.Types.AST as AST
import Relude

data Query a where
  GetMain :: Query AST.DeclarationInstance
  GetTopLevelTypes :: Query (HashMap Text AST.TopLevelTypeDecl)
  GetTopLevelControl :: Query (HashMap Text AST.P4Control)
  GetTopLevelParser :: Query (HashMap Text AST.P4Parser)
  GetTopLevelMatchKind :: Query (HashMap Text AST.DeclarationID)
  FetchType :: Text -> Query (Maybe AST.P4Type)
  GenerateP4Type :: AST.P4Type -> Query (C.TypeSpec, C.TypeSpec, [(Text, C.TypeSpec)])

deriving stock instance Show (Query a)

deriveGEq ''Query

instance Hashable (Query a) where
  hashWithSalt salt q =
    case q of
      GetMain -> h 0 ()
      GetTopLevelTypes -> h 1 ()
      GetTopLevelControl -> h 2 ()
      GetTopLevelParser -> h 3 ()
      GetTopLevelMatchKind -> h 4 ()
      FetchType t -> h 5 t
      GenerateP4Type t -> h 6 t
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
