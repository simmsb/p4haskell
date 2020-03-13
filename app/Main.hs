module Main
    ( main ) where

import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO

import qualified Df1

import           Options.Applicative

import           P4Haskell

import qualified Polysemy.Error       as P
import qualified Polysemy.Reader      as P


newtype Opts = Opts
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
  main' $ T.unpack $ input opts'

main' :: FilePath -> IO ()
main' path = do
  file <- TIO.readFile path
  r <- runM
    . P.runError
    . runDiToStderrIO
    . P.runReader file $ main''
  case r of
    Left e -> TIO.hPutStrLn stderr e
    _      -> pure ()

main''
  :: Members '[Embed IO, P.Error Text, Di Df1.Level Df1.Path Df1.Message,
  P.Reader Text] r
  => Sem r ()
main'' = do
  t <- P.ask
  -- log . show $ toEncoding ast'
  either failWithHistory print (parseAST t)
  -- log $ show parsed
  where failWithHistory (err, _hist) = do
          print err
          -- print (D.ppCursorHistory hist)
