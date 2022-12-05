module AdventOfCode.Year2022.Day3.Part1 (main) where

import AdventOfCode.Core
import Data.ByteString.Internal (c2w)
import Data.Ix (inRange)
import Relude

import qualified Data.HashSet as HashSet
import qualified Data.Text as Text

parseRucksack :: Text -> (Text, Text)
parseRucksack line = Text.splitAt (Text.length line `div` 2) line

parse :: Text -> Either Text [(Text, Text)]
parse input = Right $ fmap parseRucksack (lines input)

sharedItem :: (Text, Text) -> Either Text Char
sharedItem (l, r) =
  case HashSet.toList (HashSet.intersection (textToSet l) (textToSet r)) of
    [item] -> Right item
    items -> Left $ "More than one shared item: " <> show items
  where
  textToSet :: Text -> HashSet Char
  textToSet = Text.foldl' (flip HashSet.insert) HashSet.empty

priority :: Char -> Either Text Word8
priority c
  | inRange ('a', 'z') c = Right $ c2w c - (c2w 'a' - 1)
  | inRange ('A', 'Z') c = Right $ c2w c - (c2w 'A' - 1) + 26
  | otherwise = Left $ "Unexpected char: " <> show c

solve :: [(Text, Text)] -> Either Text Word
solve rucksacks = do
  sharedItems <- traverse sharedItem rucksacks
  priorities <- traverse priority sharedItems
  pure $ sum (fmap fromIntegral priorities)

main :: IO ()
main = run $ BasicPuzzle parse solve
