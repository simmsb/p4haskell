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
      [ "#include <stdint.h>"
      , "#include <stdlib.h>"
      , "#include <stdbool.h>"
      , "#include <arpa/inet.h>"
      , "#include <endian.h>"
      , "#include <byteswap.h>"
      , "#include <string.h>"
      , -- htonll
        "__device__ uint64_t p4_htonll(uint64_t in) {"
      , "#if __BYTE_ORDER == __LITTLE_ENDIAN"
      , "  return ((in >> 56) & 0xffull) | ((in >> 40) & 0xff00ull)"
      , "          | ((in >> 24) & 0xff0000ull)"
      , "          | ((in >> 8) & 0xff000000ull)"
      , "          | ((in & 0xff000000ull) << 8)"
      , "          | ((in & 0xff0000ull) << 24)"
      , "          | ((in & 0xff00ull) << 40)"
      , "          | ((in & 0xffull) << 56);"
      , "#else"
      , "  return in;"
      , "#endif"
      , "}"
      , -- htonl
        "__device__ uint32_t p4_htonl(uint32_t in) {"
      , "#if __BYTE_ORDER == __LITTLE_ENDIAN"
      , "  return ((in >> 24) & 0xff) | ((in >> 8) & 0xff00)"
      , "          | ((in & 0xff00) << 8) | ((in & 0xff) << 24);"
      , "#else"
      , "  return in;"
      , "#endif"
      , "}"
      , -- htons
        "__device__ uint16_t p4_htons(uint16_t in) {"
      , "#if __BYTE_ORDER == __LITTLE_ENDIAN"
      , "  return ((in >> 8) & 0xff) | ((in & 0xff) << 8);"
      , "#else"
      , "  return in;"
      , "#endif"
      , "}"
      , -- memmove
        "__device__ void p4_memmove(void *dst, void *src, size_t n) {"
      , "  char *d = (char *)dst;"
      , "  const char *s = (const char *)src;"
      , "  if ((uintptr_t)dst < (uintptr_t)src) {"
      , "    for (size_t i = 0; i < n; i++)"
      , "      d[i] = s[i];"
      , "  } else {"
      , "    for (size_t i = n; i > 0; i--)"
      , "      d[i - 1] = s[i - 1];"
      , "  }"
      , "}"
      , "int main(){}"
      ]
  putStrLn out

-- where failWithHistory (err, _hist) = do
--         print err
