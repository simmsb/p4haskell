module Main
    ( main ) where

import qualified Data.ByteString.Lazy as LB
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO

import qualified Df1                  as Df1

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
inputFile = strOption
  (long "input" <> short 'i' <> metavar "FILE" <> help "Input FILE")

opts :: ParserInfo Opts
opts = Options.Applicative.info (Opts <$> inputFile <**> helper)
  (fullDesc <> progDesc "Do stuff with FILE"
   <> header "p4haskell - test p4haskell")

main :: IO ()
main = do
  opts' <- execParser opts
  file <- TIO.readFile
    . T.unpack $ input opts'
  r <- runM
    . PE.runError
    . runDiToStderrIO
    . PR.runReader file $ main'
  case r of
    Left e -> TIO.hPutStrLn stderr e
    _      -> pure ()

main'
  :: Members '[Embed IO, PE.Error Text, Di Df1.Level Df1.Path Df1.Message,
  PR.Reader Text] r
  => Sem r ()
main' = do
  t <- PR.ask
  -- log . show $ toEncoding ast'
  either failWithHistory print (parseAST t)
  -- log $ show parsed
  where failWithHistory (err, hist) = do
          print err
          print (D.ppCursorHistory hist)
