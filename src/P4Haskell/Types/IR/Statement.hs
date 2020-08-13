-- |
module P4Haskell.Types.IR.Statement
  ( Statement (..),
    Assign (..),
    If (..),
    Switch (..),
    Return (..),
  )
where

import P4Haskell.Types.IR.Expression

data Statement
  = StatementExpr Expression
  | StatementAssign Assign
  | StatementIf If
  | StatementSwitch Switch
  | StatementReturn Return
  deriving (Generic)

data Assign = Assign
  { lvalue :: Expression,
    rvalue :: Expression
  }
  deriving (Generic)

data If = If
  { condition :: Expression,
    body :: [Statement],
    else_ :: [Statement]
  }
  deriving (Generic)

data Switch = Switch
  { condition :: Expression,
    cases :: [(Expression, [Statement])],
    def :: Maybe [Statement]
  }
  deriving (Generic)

data Return = Return
  { inner :: Expression
  }
  deriving (Generic)
