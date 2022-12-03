module AdventOfCode.Year2018.Day1.Part1 (main) where

import Prelude


parse :: String -> Int
parse ('+':is) = read is
parse ('-':is) = negate (read is)
parse _ = error "Invalid string"


main :: IO ()
main = print . sum . fmap parse . lines =<< getContents
