{-# LANGUAGE LambdaCase #-}

import Prelude hiding (fail)

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail (MonadFail(..))

data Operation
  = Add Int Int Int
  | Multiply Int Int Int
  | Halt
  deriving Show

newtype Parser a = Parser { runParser :: [Int] -> Either String ([Int], a) }

instance Functor Parser where
  fmap f (Parser p) = Parser (fmap (fmap f) . p)

instance Applicative Parser where
  pure x = Parser (const (Right ([], x)))
  Parser f <*> Parser p = undefined

instance Monad Parser where
  return = pure
  Parser p >>= f = undefined

instance Alternative Parser where
  Parser l <|> Parser r = undefined
  empty = Parser (const (Left ""))

instance MonadPlus Parser where
  mzero = empty
  mplus = (<|>)

instance MonadFail Parser where
  fail err = Parser (const (Left err))

int :: Parser Int
int = Parser (\case
  [] -> Left "Empty list"
  (x:xs) -> Right (xs, x))

operation :: Parser Operation
operation = int >>= \case
  1 -> Add <$> int <*> int <*> int
  2 -> Multiply <$> int <*> int <*> int
  99 -> pure Halt
  invalid -> fail ("Invalid operation: " <> show invalid)

parseInput :: String -> [Int]
parseInput input = read ("[" <> input <> "]")

solve :: [Int] -> Int
solve = undefined

main :: IO ()
main = print . solve . parseInput =<< getContents
