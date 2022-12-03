module AdventOfCode.Year2020.Day10.Part1 (main) where

import Relude

import qualified Data.List as List
import qualified Data.Text.IO as Text
import qualified Relude.Unsafe as Unsafe

main :: IO ()
main = do
  input <- Text.getContents
  print (solve input)

solve :: Text -> Natural
solve input = totalOnes * totalThrees
  where
    bagAdapters = input
      & lines
      & fmap (Unsafe.read @Natural . toString)
      & sort

    builtinAdapter = List.maximum bagAdapters + 3

    adapters = bagAdapters <> [builtinAdapter]

    (_, totalOnes, totalThrees) =
      List.foldl'
        (\(prev, ones, threes) adapter ->
          case adapter - prev of
            1 -> (adapter, ones + 1, threes)
            3 -> (adapter, ones, threes + 1)
            _ -> error "unreachable"
        )
        (0, 0, 0)
        adapters
