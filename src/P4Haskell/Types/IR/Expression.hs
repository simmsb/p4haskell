-- |
module P4Haskell.Types.IR.Expression
  ( Expression (..),
    Variable (..),
    Member (..),
    Index (..),
  )
where

import Prelude hiding (Index)

data Expression
  = ExpressionVariable Variable
  | ExpressionMember Member
  | ExpressionIndex Index
  deriving (Generic)

data Variable = Variable
  { name :: Text
  }
  deriving (Generic)

data Member = Member
  { lhs :: Expression,
    deref :: Bool,
    attr :: Text
  }
  deriving (Generic)

data Index = Index
  { lhs :: Expression,
    idx :: Expression
  }
  deriving (Generic)
