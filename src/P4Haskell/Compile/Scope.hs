-- | Something for keeping track of scopes
module P4Haskell.Compile.Scope
  ( Scope (..),
    Var (..),
    ScopeLookup (..),
    ParserStateInfo (..),
    lookupVarInScope,
    lookupActionInScope,
    getParserStateInfoInScope,
    runScopeLookupReader,
    makeVar,
    findVarInScope,
    findActionInScope,
    fetchParserStateInfoInScope,
    addVarToScope,
    addActionToScope,
    emptyScope,
    setParserStateInfoInScope,
  )
where

import Data.Unique
import qualified Language.C99.Simple as C
import qualified P4Haskell.Types.AST as AST
import Polysemy
import Polysemy.Fresh
import Polysemy.Reader
import Text.Show (showsPrec)

newtype VarID = VarID Int
  deriving (Show, Eq, Generic)
  deriving (Hashable)

data Var = Var
  { varOriginalName :: Text,
    varID :: VarID,
    varType :: C.Type,
    varP4Type :: AST.P4Type,
    needsDeref :: Bool
  }
  deriving (Generic)

instance Show Var where
  showsPrec i (Var name vid _ _ _) = showsPrec i $ name <> ":" <> show vid

instance Eq Var where
  (==) = on (==) (^. #varID)

instance Hashable Var where
  hashWithSalt i a = hashWithSalt i (a ^. #varID)

data ParserStateInfo = ParserStateInfo
  { psid :: Int,
    stateVar :: C.Expr,
    enumTy :: C.TypeSpec,
    states :: HashMap Text C.Expr
  }
  deriving (Generic, Show)

instance Eq ParserStateInfo where
  (==) = on (==) (^. #psid)

instance Hashable ParserStateInfo where
  hashWithSalt i a = hashWithSalt i (a ^. #psid)

data Scope = Scope
  { scopeVarBindings :: HashMap VarID Var,
    scopeVarBindingsO :: HashMap Text Var,
    scopeKnownActions :: HashMap Text AST.P4Action,
    scopeParserStateInfo :: Maybe ParserStateInfo
  }
  deriving (Show, Generic, Eq, Hashable)

emptyScope :: Scope
emptyScope = Scope mempty mempty mempty Nothing

addVarToScope :: Var -> Scope -> Scope
addVarToScope var scope =
  scope & #scopeVarBindings . at (var ^. #varID) ?~ var
    & #scopeVarBindingsO . at (var ^. #varOriginalName) ?~ var

addActionToScope :: AST.P4Action -> Scope -> Scope
addActionToScope a scope =
  scope & #scopeKnownActions . at (a ^. #name) ?~ a

setParserStateInfoInScope :: ParserStateInfo -> Scope -> Scope
setParserStateInfoInScope i scope = scope & #scopeParserStateInfo ?~ i

findVarInScope :: Text -> Scope -> Maybe Var
findVarInScope n s = s ^. #scopeVarBindingsO . at n

findActionInScope :: Text -> Scope -> Maybe AST.P4Action
findActionInScope n s = s ^. #scopeKnownActions . at n

getParserStateInfoInScope :: Scope -> Maybe ParserStateInfo
getParserStateInfoInScope s = s ^. #scopeParserStateInfo

makeVar :: Member (Fresh Unique) r => Text -> C.Type -> AST.P4Type -> Bool -> Sem r Var
makeVar n t p4t nd = do
  i <- VarID . hashUnique <$> fresh
  pure $ Var n i t p4t nd

data ScopeLookup m a where
  LookupVarInScope :: Text -> AST.P4Type -> ScopeLookup m (Maybe Var)
  LookupActionInScope :: Text -> ScopeLookup m (Maybe AST.P4Action)
  FetchParserStateInfoInScope :: ScopeLookup m (Maybe ParserStateInfo)

makeSem ''ScopeLookup

runScopeLookupReader :: Member (Reader Scope) r => Sem (ScopeLookup ': r) a -> Sem r a
runScopeLookupReader = interpret \case
  LookupVarInScope name _ty ->
    Polysemy.Reader.asks $ findVarInScope name
  LookupActionInScope name ->
    Polysemy.Reader.asks $ findActionInScope name
  FetchParserStateInfoInScope ->
    Polysemy.Reader.asks getParserStateInfoInScope
