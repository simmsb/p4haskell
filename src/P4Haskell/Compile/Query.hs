-- | Queries
module P4Haskell.Compile.Query
  ( Query (..),
    rules
  )
where

import Data.GADT.Compare.TH (deriveGEq)
import Data.Hashable
import Data.Some
import qualified Rock
import qualified P4Haskell.Types.AST as AST
import Data.Generics.Labels ()
import Data.Generics.Sum
import Relude.Unsafe (fromJust)

data Query a where
  GetMain :: Query AST.DeclarationInstance
  -- FetchType :: Text -> Query AST.P4Type

deriving instance Show (Query a)

deriveGEq ''Query

instance Hashable (Query a) where
  hashWithSalt salt q =
    case q of
      GetMain -> h 0 ()
      -- FetchType t -> h 1 t
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

rules :: AST.P4Program -> Rock.Rules Query
rules ast GetMain = (ast ^. #objects)
                    & mapMaybe (\o -> do
                                   decl <- o ^? _Typed @AST.DeclarationInstance
                                   guard (decl ^. #name == "main")
                                   pure decl)
                    & listToMaybe
                    & fromJust
                    & pure
-- rules ast (FetchType t) = (ast ^. #objects)
--                           & mapMaybe (\o -> do

--                                          )
