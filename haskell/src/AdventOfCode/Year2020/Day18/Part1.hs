module AdventOfCode.Year2020.Day18.Part1 (main) where

import AdventOfCode.Core
import AdventOfCode.Parsing
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Relude hiding (some)
import Relude.Unsafe (read)

data Expr
  = Number Integer
  | Add Expr Expr
  | Multiply Expr Expr
  deriving stock Show

parse :: Text -> Either Text [Expr]
parse = runParser do
  let lexeme = \p -> p <* hspace
  let parens = between (lexeme (char '(')) (lexeme (char ')'))
  let number = Number <$> lexeme (read <$> some digitChar)
  let ops =
        [ [ InfixL (Add <$ lexeme (char '+'))
          , InfixL (Multiply <$ lexeme (char '*'))
          ]
        ]
  let terms = parens expr <|> number
      expr = makeExprParser terms ops
  expr `sepBy1` newline <* eof

eval :: Expr -> Integer
eval = \case
  Number n -> n
  Add l r -> eval l + eval r
  Multiply l r -> eval l * eval r

solve :: [Expr] -> Either Text Integer
solve exprs = Right $ sum (eval <$> exprs)

main :: IO ()
main = run $ BasicPuzzle parse solve
