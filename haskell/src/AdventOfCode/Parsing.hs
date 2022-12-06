{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module AdventOfCode.Parsing
  ( Parser
  , runParser

  , integral
  , fractional

    -- * Re-exports
  , module Text.Megaparsec
  , module Text.Megaparsec.Char
  , module Control.Monad.Combinators.Expr
  )
where

import Control.Monad.Combinators.Expr
import Relude hiding (some)
import Relude.Unsafe (read)
import Text.Megaparsec hiding (parse, runParser)
import Text.Megaparsec.Char

import qualified Text.Megaparsec as Megaparsec

type Parser = Parsec Void Text

runParser :: Parser a -> Text -> Either Text a
runParser parser input =
  case Megaparsec.runParser parser "" input of
    Left err -> Left $ toText (errorBundlePretty err)
    Right x -> Right x

integral :: (Read a, Integral a) => Parser a
integral = read <$> some digitChar

fractional :: (Read a, Fractional a) => Parser a
fractional = asum
  [ try $ read <$> some digitChar <> (one <$> char '.') <> some digitChar
  , read <$> some digitChar
  ]
