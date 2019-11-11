module Main
    ( main ) where

import qualified Data.ByteString.Lazy as LB
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO

import           Options.Applicative

import           P4Haskell

import qualified Polysemy.Error       as PE
import qualified Polysemy.Reader      as PR

import qualified Waargonaut.Decode    as D

data Opts = Opts
  { input :: Text
  }
  deriving ( Generic )

inputFile :: Parser Text
inputFile = strOption (long "input" <> short 'i' <> metavar "FILE" <> help "Input FILE")

opts :: ParserInfo Opts
opts = info (Opts <$> inputFile <**> helper)
  (fullDesc <> progDesc "Do stuff with FILE" <> header "p4haskell - test p4haskell")

failWithHistory (err, hist) = do
  print err
  print (D.ppCursorHistory hist)

main :: IO ()
main = do
  let res = runTest . unlines $ ["{\"Node_ID\": 0, \"Node_Type\": \"Test0\", \"a\": 0, \"x\": [1, 2, 3],"
                                ,"\"y\": ["
                                ,"{\"a\": 4, \"Node_ID\": 1, \"Node_Type\": \"Test1\"}"
                                ,"{\"Node_ID\": 1},"
                                ,"],"
                                ,"}"]
  either failWithHistory print res
  pure ()

  -- opts' <- execParser opts
  -- file <- LB.readFile . T.unpack $ input opts'
  -- r <- runM . PE.runError . runLogAction logPText . PR.runReader file $ main'
  -- case r of
  --   Left e -> TIO.hPutStrLn stderr e
  --   _      -> pure ()
main' :: Members '[PE.Error Text, Log Text, PR.Reader LB.ByteString] r => Sem r ()
main' = do
  -- t <- PR.ask
  -- ast <- PE.fromEither . first T.pack $ eitherDecode t
  -- let ast' = fixupTree ast
  -- -- log . show $ toEncoding ast'
  -- parsed :: P4Program <- PE.fromEither $ fromJSON ast'
  -- -- log $ show parsed
  pure ()
