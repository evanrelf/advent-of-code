module AdventOfCode.Year2022.Day6.Part1 (main) where

import AdventOfCode.Core
import Data.List (nub)
import Relude hiding (tail)
import Relude.Unsafe (tail)

parse :: Text -> Either Text String
parse = Right . toString

solve :: String -> Either Text Int
solve = go 0
  where
  n = 4
  go _ [] = Left "Failed"
  go offset buffer =
    if length (nub (take n buffer)) == n
      then Right $ offset + n
      else go (offset + 1) (tail buffer)

main :: IO ()
main = run $ BasicPuzzle parse solve
