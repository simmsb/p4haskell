-- | P4 Annotations
module P4Haskell.Types.AST.Annotation
    ( Annotation(..)
    , parseAnnotations
    , parseAnnotation
    , AnnotatedToken
    , NamedExpression
    , parseAnnotatedToken
    , parseNamedExpression ) where

import           P4Haskell.Types.AST.Core
import           P4Haskell.Types.AST.Expression
import           P4Haskell.Types.AST.DecompressJSON

import           Prelude                            hiding ( Member )

import qualified Data.Text.Lazy                     as T

import qualified Waargonaut.Decode                  as D
import qualified Waargonaut.Encode                  as E

data Annotation = Annotation
  { name         :: Text
  , body         :: [AnnotatedToken]
  , needsParsing :: Bool
  , expr         :: [Expression]
  , kv           :: [NamedExpression]
  }
  deriving ( Show, Generic )

parseAnnotations :: DecompressC r => D.Decoder (Sem r) [Annotation]
parseAnnotations = D.withCursor . tryParseVal $ \c -> do
  o <- D.down c
  D.fromKey "annotations" (parseVector parseAnnotation) o

parseAnnotation :: DecompressC r => D.Decoder (Sem r) Annotation
parseAnnotation = D.withCursor . tryParseVal $ \c -> do
  o            <- D.down c
  name         <- D.fromKey "name" D.text o
  body         <- D.fromKey "body" (parseVector parseAnnotatedToken) o
  needsParsing <- D.fromKey "needsParsing" D.bool o
  expr         <- D.fromKey "expr" (parseVector expressionDecoder) o
  kv           <- D.fromKey "kv" (parseVector parseNamedExpression) o
  pure $ Annotation name body needsParsing expr kv

type AnnotatedToken = Text

parseAnnotatedToken :: Monad m => D.Decoder m AnnotatedToken
parseAnnotatedToken = T.toStrict . E.simplePureEncodeTextNoSpaces E.json' <$> D.json

type NamedExpression = Text

parseNamedExpression :: Monad m => D.Decoder m NamedExpression
parseNamedExpression = T.toStrict . E.simplePureEncodeTextNoSpaces E.json' <$> D.json
