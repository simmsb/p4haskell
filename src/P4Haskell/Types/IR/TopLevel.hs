-- | Toplevel IR things
module P4Haskell.Types.IR.TopLevel
  ( TopLevel (..),
    FunctionDef (..),
    TypeDef (..),
    Constant (..),
  )
where

import qualified Language.C99.Simple.AST as CAST
import P4Haskell.Types.IR.Param
import P4Haskell.Types.IR.Statement

data TopLevel
  = TopLevelFunctionDef FunctionDef
  | TopLevelTypeDef TypeDef
  | TopLevelConstant Constant
  deriving (Generic)

data FunctionDef = FunctionDef
  { name :: Text,
    annotations :: [Text],
    returnType :: CAST.Type,
    params :: [Param],
    body :: [Statement]
  }
  deriving (Generic)

data TypeDef = TypeDef
  { name :: Text,
    typ :: CAST.Type
  }
  deriving (Generic)

data Constant = Constant 
  { inner :: CAST.Decln
  }
  deriving (Generic)
