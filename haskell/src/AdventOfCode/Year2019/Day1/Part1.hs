module AdventOfCode.Year2019.Day1.Part1 (main) where

import Relude
import Relude.Unsafe (read)

import qualified Data.String as String
import qualified System.IO as IO


requiredFuel :: Int -> Int
requiredFuel mass = floor @Double (fromIntegral mass / 3) - 2


main :: IO ()
main = do
  input <- IO.getContents

  input
    & String.lines
    & fmap read
    & fmap requiredFuel
    & sum
    & print
