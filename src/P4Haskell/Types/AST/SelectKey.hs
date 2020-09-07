-- |
module P4Haskell.Types.AST.SelectKey
  ( SelectKey (..),
    selectKeyDecoder,
    DefaultExpression (..),
    parseDefaultExpression,
  )
where

import Control.Monad.Error.Class (throwError)
import Data.Generics.Product.Fields ()
import Data.Generics.Sum.Typed
import P4Haskell.Types.AST.DecompressJSON
import P4Haskell.Types.AST.Expression
import P4Haskell.Types.AST.Types
import Polysemy hiding (Member)
import qualified Waargonaut.Decode as D
import qualified Waargonaut.Decode.Error as D

data SelectKey
  = Constant'SelectKey Constant
  | Default'SelectKey DefaultExpression
  deriving ( Show, Generic, Eq, Hashable )

selectKeyDecoder :: DecompressC r => D.Decoder (Sem r) SelectKey
selectKeyDecoder = D.withCursor $ \c -> do
  nodeType <- currentNodeType c

  case nodeType of
    "Constant"          -> (_Typed @Constant #) <$> tryDecoder parseConstant c
    "DefaultExpression" -> (_Typed @DefaultExpression #) <$> tryDecoder parseDefaultExpression c
    _ -> throwError . D.ParseFailed $ "invalid node type for SelectKey: " <> nodeType

data DefaultExpression = DefaultExpression
  { type_ :: P4Type
  }
  deriving ( Show, Generic, Eq, Hashable )

parseDefaultExpression :: DecompressC r => D.Decoder (Sem r) DefaultExpression
parseDefaultExpression = D.withCursor . tryParseVal $ \c -> do
  o     <- D.down c
  type_ <- D.fromKey "type" p4TypeDecoder o
  pure $ DefaultExpression type_
