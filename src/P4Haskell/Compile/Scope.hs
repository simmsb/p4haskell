-- | Something for keeping track of scopes
module P4Haskell.Compile.Scope
  ( ScopeStack (..),
    declareVar,
    findVar,
    runScopeStack,
  )
where

import Data.Unique
import Data.Generics.Labels ()
import Polysemy
import Polysemy.Fresh
import Polysemy.Internal.Tactics
import Polysemy.Reader as P

newtype VarID = VarID Int
  deriving (Show, Eq, Generic)
  deriving newtype (Hashable)

data Var = Var
  { varOriginalName :: Text,
    varID :: VarID,
    varType :: () -- TODO
  }
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

data ScopeStack m a where
  DeclareVar :: Text -> () -> (Var -> m a) -> ScopeStack m a
  FindVar :: Text -> ScopeStack m (Maybe Var)

makeSem ''ScopeStack

runScopeStack :: Member (Embed IO) r => Sem (ScopeStack ': r) a -> Sem r a
runScopeStack =
  runReader emptyScope
    . freshToIO
    . reinterpret2H \case
      DeclareVar n t m -> do
        i <- VarID . hashUnique <$> fresh
        let var = Var n i t
        mx <- runT $ m var
        raise . runScopeStack $ P.local (addToScope var) mx
      FindVar n ->
        liftT $ P.asks (findInScope n)
