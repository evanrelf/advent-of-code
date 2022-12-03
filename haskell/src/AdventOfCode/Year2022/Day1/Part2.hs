module AdventOfCode.Year2022.Day1.Part2 (main) where

import AdventOfCode.Core
import Relude

import qualified Data.Text as Text

main :: IO ()
main = runPuzzleIO (Puzzle parse solve)

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
  & sortOn Down
  & take 3
  & sum
  & Right
