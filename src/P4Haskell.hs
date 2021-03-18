module P4Haskell (
  module P4Haskell.Types.AST,
  parseAST,
) where

import qualified Data.Attoparsec.Text as AT
import P4Haskell.Types.AST
import Relude
import Waargonaut.Decode.Runners
import Waargonaut.Decode.Error
import Waargonaut.Decode.Types

parseAST :: Text -> Either (DecodeError, CursorHistory) P4Program
parseAST input = runDecompressor $ decodeFromText AT.parseOnly astDecoder input
