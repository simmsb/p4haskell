-- | Represents the P4 AST in haskell
module P4Haskell.Types.AST
  ( astDecoder
  ) where

import qualified Data.HashMap.Lazy              as H

import           P4Haskell.Types.DecompressJSON

import           Waargonaut
import qualified Waargonaut.Decode              as D

type DecompressC' r = DecompressC Node r

nodeDecoder :: DecompressC' r => D.Decoder (Sem r) Node
nodeDecoder = D.withCursor $ \curs -> do
  pure Nope

astDecoder :: DecompressC' r => D.Decoder (Sem r) P4Program
astDecoder = D.withCursor $ \curs -> do
  pure $ P4Program { objects = [Nope] }

newtype P4Program = P4Program
  { objects :: [Node]
  }
  deriving ( Show, Generic )

data Node
  = TypeError' TypeError
  | TypeExtern' TypeExtern
  | Nope
  deriving ( Show, Generic )

newtype Annotation = Annotation
  { aa :: Text
  }
  deriving ( Show, Generic )

newtype Annotations = Annotations
  { annotations :: [Annotation]
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
  { parameters :: [TypeVar']
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
  { parameters :: [Parameter]
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

data TypeExtern = TypeExtern
  { annotations    :: Annotations
  , typeParameters :: TypeParameters
  , methods        :: [Method]
  , name           :: Text
  , attributes     :: HashMap Text Attribute
  }
  deriving ( Show, Generic )

data TypeError = TypeError
  { members :: [DeclarationID]
  , name    :: Text
  , declid  :: Integer
  }
  deriving ( Show, Generic )

newtype DeclarationID = DeclarationID
  { name :: Text
  }
  deriving ( Show, Generic )
