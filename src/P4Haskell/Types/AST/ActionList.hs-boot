module P4Haskell.Types.AST.ActionList
  ( ActionList,
    ActionListElement,
    parseActionList,
    parseActionListElement,
  )
where

import P4Haskell.Types.AST.DecompressJSON
import Polysemy
import Relude
import qualified Waargonaut.Decode as D

data ActionList

instance Show ActionList

instance Eq ActionList

instance Hashable ActionList

parseActionList :: DecompressC r => D.Decoder (Sem r) ActionList

data ActionListElement

instance Show ActionListElement

instance Eq ActionListElement

instance Hashable ActionListElement

parseActionListElement :: DecompressC r => D.Decoder (Sem r) ActionListElement
