module AdventOfCode.Year2020.Day6.Part2 (main) where

import Relude hiding (group)

import qualified Data.List as List
import qualified Data.String as String
import qualified System.IO as IO


group :: [String] -> [[String]]
group = fmap (filter (/= "")) . List.groupBy (\_line1 line2 -> line2 /= "")


everyone :: [String] -> String
everyone = List.foldl1' $ \s1 s2 ->
  let
    both = List.nub $ sort $ s1 <> s2
  in
    filter (\l -> l `elem` s1 && l `elem` s2) both


main :: IO ()
main = do
  input <- IO.getContents

  input
    & String.lines
    & group
    & fmap everyone
    & fmap length
    & sum
    & print
