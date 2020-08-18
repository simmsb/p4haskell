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
import Polysemy
import Polysemy.Fresh

newtype VarID = VarID Int
  deriving (Show, Eq, Generic)
  deriving (Hashable)

data Var = Var
  { varOriginalName :: Text,
    varID :: VarID,
    varType :: ()
  }
  -- TODO

  deriving (Show, Generic, Eq, Hashable)

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

makeVar :: Member (Fresh Unique) r => Text -> () -> Sem r Var
makeVar n t = do
  i <- VarID . hashUnique <$> fresh
  pure $ Var n i t
