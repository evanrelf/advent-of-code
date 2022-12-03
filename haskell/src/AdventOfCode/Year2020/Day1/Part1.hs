module AdventOfCode.Year2020.Day1.Part1 (main) where

import Relude
import Relude.Unsafe (read)

import qualified Data.String as String
import qualified System.IO as IO


main :: IO ()
main = do
  input <- IO.getContents

  let entries :: [Integer]
      entries = fmap read . String.lines $ input

  let bruteForce = do
        x <- entries
        y <- entries
        guard (x + y == 2020)
        pure (x * y)

  case bruteForce of
    (x : _) -> print x
    [] -> die "Couldn't find solution"
