module AdventOfCode.Year2018.Day2.Part2 (main) where

import Control.Monad (guard)
import Data.List (nub)
import Data.Maybe (catMaybes)
import Prelude


similar :: String -> String -> Bool
similar id1 id2 = length (commonChars id1 id2) == length id1 - 1


find :: [String] -> [(String, String)]
find ids = do
  id1 <- ids
  id2 <- ids
  guard (similar id1 id2)
  pure (id1, id2)


commonChars :: String -> String -> String
commonChars id1 id2 =
  catMaybes (zipWith (\x y -> if x == y then Just x else Nothing) id1 id2)


main :: IO ()
main = print . nub . fmap (uncurry commonChars) . find . lines =<< getContents
