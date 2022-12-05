module AdventOfCode.Year2022.Day4.Part1 (main) where

import AdventOfCode.Core
import AdventOfCode.Parsing
import Relude hiding (some, id)
import Relude.Unsafe (read)
import Witherable (filterA)

data Range = Range Int Int
  deriving stock (Show)

data Pair = Pair Range Range
  deriving stock (Show)

parse :: Text -> Either Text [Pair]
parse = runParser do
  let id = read <$> some digitChar

  let range = do
        x <- id
        _ <- char '-'
        y <- id
        pure $ Range x y

  let pair = do
        x <- range
        _ <- char ','
        y <- range
        pure $ Pair x y

  pair `sepBy` newline

overlapping :: Pair -> Either Text Bool
overlapping pair@(Pair (Range w x) (Range y z)) =
  if w > x || y > z
    then Left $ "Invalid range(s) in pair: " <> show pair
    else Right $ (y <= w && x <= z) || (w <= y && z <= x)

solve :: [Pair] -> Either Text Int
solve pairs = length <$> filterA overlapping pairs

main :: IO ()
main = run $ BasicPuzzle parse solve
