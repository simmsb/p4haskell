-- | P4 action param directions
module P4Haskell.Types.IR.Param
  ( Direction (..),
    Param (..),
  )
where

import qualified Language.C99.Simple.AST as CAST

data Direction = Direction
  { isPacket :: Bool,
    dirIn :: Bool,
    dirOut :: Bool
  }
  deriving (Show, Generic)

data Param = Param
  { dir :: Direction,
    -- | NOTE: we probably add a pointer to the type if the direction is a for of 'out'
    typ :: CAST.Type,
    name :: Text
  }
  deriving (Generic)
