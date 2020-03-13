module P4Haskell
    ( module P4Haskell.Types.AST
    , parseAST ) where

import qualified Data.Attoparsec.Text           as AT

import           P4Haskell.Types.AST

import           Waargonaut.Decode.Runners

parseAST :: _
parseAST = runDecompressor . decodeFromText AT.parseOnly astDecoder
