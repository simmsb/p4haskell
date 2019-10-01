-- | Represents the P4 AST in haskell
module P4Haskell.Types.AST
    (
     ) where

import           Data.Aeson

jsonOptions :: Options
jsonOptions = defaultOptions { sumEncoding        = TaggedObject "Node_Type" "Node_Type"
                             , omitNothingFields  = True
                             , fieldLabelModifier = filter (/= '_') }
