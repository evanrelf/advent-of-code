module AdventOfCode.Parsing
  ( Parser
  , runParser

    -- * Re-exports
  , module Text.Megaparsec
  , module Text.Megaparsec.Char
  , module Control.Monad.Combinators.Expr
  )
where

import Control.Monad.Combinators.Expr
import Relude
import Text.Megaparsec hiding (parse, runParser)
import Text.Megaparsec.Char

import qualified Text.Megaparsec as Megaparsec

type Parser = Parsec Void Text

runParser :: Parser a -> Text -> Either Text a
runParser parser input =
  case Megaparsec.runParser parser "" input of
    Left err -> Left $ toText (errorBundlePretty err)
    Right x -> Right x
