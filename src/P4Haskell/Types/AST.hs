-- | Represents the P4 AST in haskell
module P4Haskell.Types.AST where

import           Data.Aeson
import           Data.Aeson.Types  ( prependFailure )
import qualified Data.HashMap.Lazy as H
import           Data.Typeable     ( typeRep )

import           GHC.Generics

jsonOptions :: Options
jsonOptions = defaultOptions { sumEncoding            = TaggedObject "p4haskell_type" "p4haskell_content"
                             , omitNothingFields      = True
                             , fieldLabelModifier     = filter (/= '_')
                             , constructorTagModifier = filter (/= '_') }

newtype P4JSON a = P4JSON
  { unP4JSON :: a
  }

instance (Typeable a, Generic a, GToJSON Zero (Rep a), GToEncoding Zero (Rep a)) => ToJSON (P4JSON a) where
  toJSON = genericToJSON jsonOptions . unP4JSON

  toEncoding = genericToEncoding jsonOptions . unP4JSON

instance (Typeable a, Generic a, GFromJSON Zero (Rep a)) => FromJSON (P4JSON a) where
  parseJSON = prependFailure ((<> " ") . show . typeRep $ Proxy @a) . fmap (P4JSON . traceShow "lol") . genericParseJSON jsonOptions

newtype P4JSONSum a = P4JSONSum
  { unP4JSONSum :: a
  }

packSumTypesInner :: Value -> Value
packSumTypesInner (Object o) = fromMaybe (Object o) $ do
  t <- H.lookup "Node_Type" o
  let rest = H.delete "Node_type" o
  pure . Object $ H.fromList [("p4haskell_type", t), ("p4haskell_content", Object rest)]
packSumTypesInner x = x

instance (Typeable a, Generic a, GFromJSON Zero (Rep a)) => FromJSON (P4JSONSum a) where
  parseJSON = prependFailure ((<> " ") . show . typeRep $ Proxy @a) . fmap P4JSONSum . genericParseJSON jsonOptions
    . packSumTypesInner

instance (Typeable a, Generic a, GToJSON Zero (Rep a), GToEncoding Zero (Rep a)) => ToJSON (P4JSONSum a) where
  toJSON = genericToJSON jsonOptions . unP4JSONSum

  toEncoding = genericToEncoding jsonOptions . unP4JSONSum

data Node
  = TypeError TypeError'
  | TypeExtern TypeExtern'
  deriving ( Show, Generic )
  deriving ( FromJSON, ToJSON ) via P4JSONSum Node

newtype P4Program = P4Program
  { objects :: Vector Node
  }
  deriving ( Show, Generic )
  deriving ( FromJSON, ToJSON ) via P4JSON P4Program

newtype Annotation = Annotation
  { aa :: Text
  }
  deriving ( Show, Generic )
  deriving ( FromJSON, ToJSON ) via P4JSON Annotation

newtype Annotations = Annotations
  { annotations :: Vector Annotation
  }
  deriving ( Show, Generic )
  deriving ( FromJSON, ToJSON ) via P4JSON Annotations

newtype IndexedVector a = IndexedVector
  { declarations :: H.HashMap Text a
  }
  deriving ( Show, Generic )
  deriving ( FromJSON, ToJSON ) via P4JSON (IndexedVector a)

newtype NameMap a = NameMap
  { symbols :: H.HashMap Text a
  }
  deriving ( Show, Generic )
  deriving ( FromJSON, ToJSON ) via P4JSON (NameMap a)

newtype Vector a = Vector
  { vec :: [a]
  }
  deriving ( Show, Generic )
  deriving ( FromJSON, ToJSON ) via P4JSON (Vector a)

data P4Type
  = TypeVar TypeVar'
  | TypeVoid TypeVoid'
  | TypeBits TypeBits'
  deriving ( Show, Generic )
  deriving ( FromJSON, ToJSON ) via P4JSONSum P4Type

newtype TypeVoid' = TypeVoid'
  { empty :: Maybe Int } -- HACK: Aeson thinks this should be an array for some reason
  deriving ( Show, Generic )
  deriving ( FromJSON, ToJSON ) via P4JSON TypeVoid'

data TypeBits' = TypeBits'
  { size     :: Integer
  , isSigned :: Bool
  }
  deriving ( Show, Generic )
  deriving ( FromJSON, ToJSON ) via P4JSON TypeBits'

newtype TypeVar' = TypeVar'
  { name :: Text
  }
  deriving ( Show, Generic )
  deriving ( FromJSON, ToJSON ) via P4JSON TypeVar'

newtype TypeParameters = TypeParameters
  { parameters :: IndexedVector TypeVar'
  }
  deriving ( Show, Generic )
  deriving ( FromJSON, ToJSON ) via P4JSON TypeParameters

data Path = Path
  { absolute :: Bool
  , name     :: Text
  }
  deriving ( Show, Generic )
  deriving ( FromJSON, ToJSON ) via P4JSON Path

newtype TypeName = TypeName
  { path :: Path
  }
  deriving ( Show, Generic )
  deriving ( FromJSON, ToJSON ) via P4JSON TypeName

data Parameter = Parameter
  { annotations :: Annotations
  , direction   :: Text
  , name        :: Text
  , type_       :: TypeName
  }
  deriving ( Show, Generic )
  deriving ( FromJSON, ToJSON ) via P4JSON Parameter

newtype ParameterList = ParameterList
  { parameters :: IndexedVector Parameter
  }
  deriving ( Show, Generic )
  deriving ( FromJSON, ToJSON ) via P4JSON ParameterList

data TypeMethod = TypeMethod
  { typeParameters :: TypeParameters
  , parameters     :: ParameterList
  , returnType     :: P4Type
  }
  deriving ( Show, Generic )
  deriving ( FromJSON, ToJSON ) via P4JSON TypeMethod

data Method = Method
  { annotations :: Annotations
  , name        :: Text
  , type_       :: TypeMethod
  }
  deriving ( Show, Generic )
  deriving ( FromJSON, ToJSON ) via P4JSON Method

newtype Attribute = Attribute Value
  deriving ( Show, Generic )
  deriving ( FromJSON, ToJSON ) via P4JSON Attribute

data TypeExtern' = TypeExtern'
  { annotations    :: Annotations
  , typeParameters :: TypeParameters
  , methods        :: Vector Method
  , name           :: Text
  , attributes     :: NameMap Attribute
  }
  deriving ( Show, Generic )
  deriving ( FromJSON, ToJSON ) via P4JSON TypeExtern'

data TypeError' = TypeError'
  { members :: IndexedVector DeclarationID
  , name    :: Text
  , declid  :: Integer
  }
  deriving ( Show, Generic )
  deriving ( FromJSON, ToJSON ) via P4JSON TypeError'

newtype DeclarationID = DeclarationID
  { name :: Text
  }
  deriving ( Show, Generic )
  deriving ( FromJSON, ToJSON ) via P4JSON DeclarationID
