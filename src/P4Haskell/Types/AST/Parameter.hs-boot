module P4Haskell.Types.AST.Parameter
    ( Parameter
    , parseParameter ) where

import           P4Haskell.Types.AST.DecompressJSON

import           Polysemy

import qualified Waargonaut.Decode                  as D

data Parameter

instance Show Parameter

parseParameter :: DecompressC r => D.Decoder (Sem r) Parameter
