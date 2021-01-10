-- | Compilation options
module P4Haskell.Compile.Opts (
    Opts (..),
) where

import Relude

newtype Opts = Opts
    { cpuMode :: Bool
    }
    deriving stock (Generic)
