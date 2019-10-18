module Main
    ( main ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.IO         as TIO
import qualified Data.Text            as T

import           Options.Applicative

import           P4Haskell

import qualified Polysemy.Error       as PE
import qualified Polysemy.Reader      as PR

data Opts = Opts
  { input :: Text
  }
  deriving ( Generic )

inputFile :: Parser Text
inputFile = strOption (long "input" <> short 'i' <> metavar "FILE" <> help "Input FILE")

opts :: ParserInfo Opts
opts = info (Opts <$> inputFile <**> helper)
  (fullDesc <> progDesc "Do stuff with FILE" <> header "p4haskell - test p4haskell")

main :: IO ()
main = do
  opts' <- execParser opts
  file <- LB.readFile . T.unpack $ input opts'
  r <- runM . PE.runError . runLogAction logPText . PR.runReader file $ main'
  case r of
    Left e -> TIO.hPutStrLn stderr e
    _      -> pure ()

fromResult :: Result a -> Either Text a
fromResult (Data.Aeson.Error s) = Left $ T.pack s
fromResult (Data.Aeson.Success a) = Right a

main' :: Members '[PE.Error Text, Log Text, PR.Reader LB.ByteString] r => Sem r ()
main' = do
  t <- PR.ask
  ast <- PE.fromEither . first T.pack $ eitherDecode t
  let ast' = fixupTree ast
  -- log . show $ toEncoding ast'
  parsed :: P4Program <- PE.fromEither . fromResult $ fromJSON ast'
  -- log $ show parsed
  pure ()
