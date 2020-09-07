-- |
module P4Haskell.Types.AST.Table
  ( P4Table
  , parseP4Table
  , Key
  , parseKey
  , KeyElement
  , parseKeyElement
  , ExpressionValue
  , parseExpressionValue ) where

import           P4Haskell.Types.AST.DecompressJSON

import Polysemy hiding (Member)

import qualified Waargonaut.Decode as D

data P4Table

instance Show P4Table

instance Eq P4Table

instance Hashable P4Table


parseP4Table :: DecompressC r => D.Decoder (Sem r) P4Table

data Key

instance Show Key

instance Eq Key

instance Hashable Key

parseKey :: DecompressC r => D.Decoder (Sem r) Key

data KeyElement

instance Show KeyElement

instance Eq KeyElement

instance Hashable KeyElement


parseKeyElement :: DecompressC r => D.Decoder (Sem r) KeyElement

data ExpressionValue

instance Show ExpressionValue

instance Eq ExpressionValue

instance Hashable ExpressionValue

parseExpressionValue :: DecompressC r => D.Decoder (Sem r) ExpressionValue
