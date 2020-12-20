module P4Haskell.Types.AST.Method
    ( Method
    , parseMethod ) where

import           P4Haskell.Types.AST.DecompressJSON

import Relude
import           Polysemy

import qualified Waargonaut.Decode                  as D

data Method

instance Show Method

instance Eq Method

instance Hashable Method

parseMethod :: DecompressC r => D.Decoder (Sem r) Method
