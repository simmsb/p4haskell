-- | Represents the P4 AST in haskell
module P4Haskell.Types.AST where

import           Data.Generics.Sum.Constructors
import qualified Data.HashMap.Lazy              as H

import           P4Haskell.Types.DecompressJSON

import           Waargonaut
import qualified Waargonaut.Decode              as D

type DecompressC' r = DecompressC TestNode r

data TestNode
  = Test0Node Test0
  | Test1Node Test1
  deriving ( Show, Generic )

data Test0 = Test0
  { x :: [Int]
  , y :: [Test1]
  }
  deriving ( Show, Generic )

test0Decoder :: DecompressC' r => D.Decoder (Sem r) Test0
test0Decoder = D.withCursor . tryParseVal (_Ctor @"Test0Node") $ \curs -> mdo
  o <- D.down curs
  x <- D.fromKey "x" (D.list D.int) o
  y <- D.fromKey "y" (D.list test1Decoder) o
  pure $ Test0 x y

data Test1 = Test1
  { a :: Int
  }
  deriving ( Show, Generic )

test1Decoder :: DecompressC' r => D.Decoder (Sem r) Test1
test1Decoder = D.withCursor . tryParseVal (_Ctor @"Test1Node") $ \curs -> mdo
  o <- D.down curs
  a <- D.fromKey "a" D.int o
  pure $ Test1 a

newtype P4Program = P4Program
  { objects :: Vector Node
  }
  deriving ( Show, Generic )

data Node
  = TypeError TypeError'
  | TypeExtern TypeExtern'
  deriving ( Show, Generic )

newtype Annotation = Annotation
  { aa :: Text
  }
  deriving ( Show, Generic )

newtype Annotations = Annotations
  { annotations :: Vector Annotation
  }
  deriving ( Show, Generic )

newtype IndexedVector a = IndexedVector
  { declarations :: H.HashMap Text a
  }
  deriving ( Show, Generic )

newtype NameMap a = NameMap
  { symbols :: H.HashMap Text a
  }
  deriving ( Show, Generic )

newtype Vector a = Vector
  { vec :: [a]
  }
  deriving ( Show, Generic )

data P4Type
  = TypeVar TypeVar'
  | TypeVoid TypeVoid'
  | TypeBits TypeBits'
  deriving ( Show, Generic )

data TypeVoid' = TypeVoid'
  deriving ( Show, Generic )

data TypeBits' = TypeBits'
  { size     :: Integer
  , isSigned :: Bool
  }
  deriving ( Show, Generic )

newtype TypeVar' = TypeVar'
  { name :: Text
  }
  deriving ( Show, Generic )

newtype TypeParameters = TypeParameters
  { parameters :: IndexedVector TypeVar'
  }
  deriving ( Show, Generic )

data Path = Path
  { absolute :: Bool
  , name     :: Text
  }
  deriving ( Show, Generic )

newtype TypeName = TypeName
  { path :: Path
  }
  deriving ( Show, Generic )

data Parameter = Parameter
  { annotations :: Annotations
  , direction   :: Text
  , name        :: Text
  , type_       :: TypeName
  }
  deriving ( Show, Generic )

newtype ParameterList = ParameterList
  { parameters :: IndexedVector Parameter
  }
  deriving ( Show, Generic )

data TypeMethod = TypeMethod
  { typeParameters :: TypeParameters
  , parameters     :: ParameterList
  , returnType     :: P4Type
  }
  deriving ( Show, Generic )

data Method = Method
  { annotations :: Annotations
  , name        :: Text
  , type_       :: TypeMethod
  }
  deriving ( Show, Generic )

newtype Attribute = Attribute Json
  deriving ( Show, Generic )

data TypeExtern' = TypeExtern'
  { annotations    :: Annotations
  , typeParameters :: TypeParameters
  , methods        :: Vector Method
  , name           :: Text
  , attributes     :: NameMap Attribute
  }
  deriving ( Show, Generic )

data TypeError' = TypeError'
  { members :: IndexedVector DeclarationID
  , name    :: Text
  , declid  :: Integer
  }
  deriving ( Show, Generic )

newtype DeclarationID = DeclarationID
  { name :: Text
  }
  deriving ( Show, Generic )
