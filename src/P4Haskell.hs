module P4Haskell
    ( someFunc
    , module P4Haskell.Types.P4AST ) where

import           P4Haskell.Types.P4AST

aaa :: Member (Log Text) r => Sem r ()
aaa = do
  log "aaa"
  log "bbb"

someFunc :: IO ()
someFunc = runM $ runLogAction logTextStdout aaa
