module P4Haskell
    ( module P4Haskell.Types.AST
    , runTest
    ) where

import           P4Haskell.Types.AST
import           P4Haskell.Types.DecompressJSON

import Waargonaut.Decode.Runners
import qualified Data.Attoparsec.Text as AT

runTest :: _
runTest = runDecompressor . decodeFromText AT.parseOnly test0Decoder
