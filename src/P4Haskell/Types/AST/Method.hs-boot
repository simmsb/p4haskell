module P4Haskell.Types.AST.Method
    ( Method(..)
    , parseMethod ) where

import           P4Haskell.Types.AST.DecompressJSON

import qualified Waargonaut.Decode                  as D

data Method

instance Show Method

parseMethod :: DecompressC r => D.Decoder (Sem r) Method
