module P4Haskell.Types.AST.Parameter
    ( Parameter
    , Direction
    , parseParameter
    , parseDirection ) where

import           P4Haskell.Types.AST.DecompressJSON

import           Polysemy

import qualified Waargonaut.Decode                  as D

data Direction

instance Show Direction

instance Eq Direction

instance Hashable Direction

parseDirection :: Monad m => D.Decoder m Direction

data Parameter

instance Show Parameter

instance Eq Parameter

instance Hashable Parameter

parseParameter :: DecompressC r => D.Decoder (Sem r) Parameter
