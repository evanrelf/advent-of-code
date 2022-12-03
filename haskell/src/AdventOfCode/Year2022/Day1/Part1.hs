module AdventOfCode.Year2022.Day1.Part1 (main) where

import AdventOfCode.Core
import Relude

import qualified Data.List as List
import qualified Data.Text as Text

main :: IO ()
main = run $ BasicPuzzle parse solve

parse :: Text -> Either Text [[Word]]
parse input =
  input
  & Text.strip
  & Text.splitOn "\n\n"
  & fmap Text.lines
  & traverse (traverse (toString >>> readEither @Word))

solve :: [[Word]] -> Either Text Word
solve inventories =
  inventories
  & fmap sum
  & List.maximum
  & Right
