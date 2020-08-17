-- | Something for keeping track of scopes
module P4Haskell.Compile.Scope
  ( Scope (..),
    Var (..),
    findInScope,
    makeVar,
    addToScope,
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

  deriving (Show, Generic)

data Scope = Scope
  { scopeBindings :: HashMap VarID Var,
    scopeBindingsO :: HashMap Text Var
  }
  deriving (Show, Generic)

emptyScope :: Scope
emptyScope = Scope mempty mempty

addToScope :: Var -> Scope -> Scope
addToScope var scope =
  scope & #scopeBindings . at (var ^. #varID) ?~ var
    & #scopeBindingsO . at (var ^. #varOriginalName) ?~ var

findInScope :: Text -> Scope -> Maybe Var
findInScope n s = s ^. #scopeBindingsO . at n

makeVar :: Member (Fresh Unique) r => Text -> () -> Sem r Var
makeVar n t = do
  i <- VarID . hashUnique <$> fresh
  pure $ Var n i t
