module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Language.C99.Pretty as PC
import qualified Language.C99.Simple as C
import Options.Applicative
import P4Haskell
import qualified P4Haskell.Compile.Codegen as CG
import qualified P4Haskell.Compile.Declared as D
import qualified P4Haskell.Compile.Eff as E
import qualified P4Haskell.Compile.Rules as R
import qualified Polysemy as P
import Relude
import qualified Text.PrettyPrint as TP

-- import qualified Waargonaut.Decode    as D

newtype Opts = Opts
  { input :: Text
  }
  deriving stock (Generic)

inputFile :: Parser Text
inputFile =
  strOption
    (long "input" <> short 'i' <> metavar "FILE" <> help "Input FILE")

opts :: ParserInfo Opts
opts =
  Options.Applicative.info
    (Opts <$> inputFile <**> helper)
    ( fullDesc <> progDesc "Do stuff with FILE"
        <> header "p4haskell - test p4haskell"
    )

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
  let parsed = parseAST t

  let Right ast = parsed

  (declared, ()) <- P.runFinal . P.embedToFinal . E.runComp (R.rules ast) ast $ CG.generateMain

  let out = TP.render . PC.pretty . C.translate . D.exportDeclared $ declared
  putTextLn $
    unlines
      [ "#include <stdint.h>",
        "#include <stdlib.h>",
        "#include <stdbool.h>",
        "#include <arpa/inet.h>",
        "#include <endian.h>",
        "#include <byteswap.h>",
        "#include <string.h>",
        "uint64_t htonll(uint64_t in) {",
        "#if __BYTE_ORDER == __LITTLE_ENDIAN",
        "  return bswap_64(in);",
        "#else",
        "  return in;",
        "#endif",
        "}",
        "int main(){}"
      ]
  putStrLn out

-- where failWithHistory (err, _hist) = do
--         print err
