-- | P4 Parameters
module P4Haskell.Types.AST.Parameter
  ( Parameter (..),
    Direction (..),
    parseParameter,
    parseDirection,
  )
where

import P4Haskell.Types.AST.Annotation
import P4Haskell.Types.AST.DecompressJSON
import P4Haskell.Types.AST.Types
import Polysemy
import Relude
import qualified Waargonaut.Decode as D

data Direction = Direction
  { in_ :: Bool
  , out :: Bool
  }
  deriving ( Show, Generic, Eq, Hashable )

parseDirection :: Monad m => D.Decoder m Direction
parseDirection = D.text <&> \case
  "in"    -> Direction True  False
  "out"   -> Direction False True
  "inout" -> Direction True  True
  _       -> Direction False False

data Parameter = Parameter
  { annotations :: [Annotation]
  , direction   :: Direction
  , name        :: Text
  , type_       :: P4Type
  }
  deriving ( Show, Generic, Eq, Hashable )

parseParameter :: DecompressC r => D.Decoder (Sem r) Parameter
parseParameter = D.withCursor . tryParseVal $ \c -> do
  o <- D.down c
  annotations <- D.fromKey "annotations" parseAnnotations o
  direction   <- D.fromKey "direction" parseDirection o
  name        <- D.fromKey "name" D.text o
  type_       <- D.fromKey "type" p4TypeDecoder o
  pure $ Parameter annotations direction name type_
