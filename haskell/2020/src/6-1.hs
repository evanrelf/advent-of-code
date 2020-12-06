module Main (main) where

import Prelude hiding (group)

import qualified Data.List as List
import qualified Data.String as String
import qualified System.IO as IO


group :: [String] -> [[String]]
group = fmap (filter (/= "")) . List.groupBy (\_line1 line2 -> line2 /= "")


main :: IO ()
main = do
  input <- IO.getContents

  input
    & String.lines
    & group
    & fmap mconcat
    & fmap sort
    & fmap List.nub
    & fmap length
    & sum
    & print
