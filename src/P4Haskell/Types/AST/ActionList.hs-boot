module P4Haskell.Types.AST.ActionList
    ( ActionList
    , ActionListElement
    , parseActionList
    , parseActionListElement ) where

import           P4Haskell.Types.AST.DecompressJSON

import           Polysemy

import qualified Waargonaut.Decode                  as D

data ActionList

instance Show ActionList

parseActionList :: DecompressC r => D.Decoder (Sem r) ActionList

data ActionListElement

instance Show ActionListElement

parseActionListElement :: DecompressC r => D.Decoder (Sem r) ActionListElement
