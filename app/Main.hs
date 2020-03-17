module Main
    ( main ) where

import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO

import           Options.Applicative

import           P4Haskell

import Text.Pretty.Simple (pPrint)

-- import qualified Waargonaut.Decode    as D


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
  main'' file

main'' :: Text -> IO ()
main'' t = do
  -- log . show $ toEncoding ast'
  let parsed = parseAST t
  either failWithHistory pPrint parsed
  -- log $ show parsed
  where failWithHistory (err, _hist) = do
          print err
          -- print (D.ppCursorHistory hist)
