module P4Haskell.Types.AST.DeclarationID
  ( DeclarationID (..),
    parseDeclarationID,
  )
where

import P4Haskell.Types.AST.DecompressJSON
import Polysemy
import Relude
import qualified Waargonaut.Decode as D

data DeclarationID = DeclarationID
  { name   :: Text
  , declID :: Int
  }
  deriving ( Show, Generic, Eq, Hashable )

parseDeclarationID :: DecompressC r => D.Decoder (Sem r) DeclarationID
parseDeclarationID = D.withCursor . tryParseVal $ \c -> do
  o      <- D.down c
  name   <- D.fromKey "name" D.text o
  declID <- D.fromKey "declid" D.int o
  pure $ DeclarationID name declID
