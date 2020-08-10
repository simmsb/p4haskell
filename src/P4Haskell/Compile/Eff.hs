-- |
module P4Haskell.Compile.Eff
  ( CompC,
  )
where

import P4Haskell.Compile.Declared
import P4Haskell.Compile.Fetch
import P4Haskell.Compile.Query
import Polysemy
import Polysemy.Writer

type CompC r =
  Members
    [ Fetch Query,
      Writer Declared
    ]
    r
