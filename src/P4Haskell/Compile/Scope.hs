-- | Something for keeping track of scopes
module P4Haskell.Compile.Scope
  ( Scope (..),
    Var (..),
    makeVar,
    findVarInScope,
    addVarToScope,
    emptyScope,
  )
where

import Data.Unique
import qualified Language.C99.Simple as C
import Polysemy
import Polysemy.Fresh
import Text.Show (showsPrec)

newtype VarID = VarID Int
  deriving (Show, Eq, Generic)
  deriving (Hashable)

data Var = Var
  { varOriginalName :: Text,
    varID :: VarID,
    varType :: C.Type,
    needsDeref :: Bool
  }
  deriving (Generic)

instance Show Var where
  showsPrec i (Var name vid _ _) = showsPrec i $ name <> ":" <> show vid

instance Eq Var where
  (==) = on (==) (^. #varID)

instance Hashable Var where
  hashWithSalt i a = hashWithSalt i (a ^. #varID)

data Scope = Scope
  { scopeVarBindings :: HashMap VarID Var,
    scopeVarBindingsO :: HashMap Text Var
  }
  deriving (Show, Generic, Eq, Hashable)

emptyScope :: Scope
emptyScope = Scope mempty mempty

addVarToScope :: Var -> Scope -> Scope
addVarToScope var scope =
  scope & #scopeVarBindings . at (var ^. #varID) ?~ var
    & #scopeVarBindingsO . at (var ^. #varOriginalName) ?~ var

findVarInScope :: Text -> Scope -> Maybe Var
findVarInScope n s = s ^. #scopeVarBindingsO . at n

makeVar :: Member (Fresh Unique) r => Text -> C.Type -> Bool -> Sem r Var
makeVar n t nd = do
  i <- VarID . hashUnique <$> fresh
  pure $ Var n i t nd
