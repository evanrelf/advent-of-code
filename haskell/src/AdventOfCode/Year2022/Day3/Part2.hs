module AdventOfCode.Year2022.Day3.Part2 (main) where

import AdventOfCode.Core
import Data.ByteString.Internal (c2w)
import Data.Ix (inRange)
import Data.List.Split (chunksOf)
import Relude

import qualified Data.HashSet as HashSet
import qualified Data.Text as Text

parse :: Text -> Either Text [(Text, Text, Text)]
parse input =
  input
  & lines
  & chunksOf 3
  & traverse \case
      [x, y, z] -> Right (x, y, z)
      groups -> Left $ "Fewer than 3 rucksacks in a group:\n" <> show groups

sharedItem :: (Text, Text, Text) -> Either Text Char
sharedItem (x, y, z) =
  case
    HashSet.toList $
      textToSet x
      `HashSet.intersection`
      textToSet y
      `HashSet.intersection`
      textToSet z
  of
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

solve :: [(Text, Text, Text)] -> Either Text Word
solve rucksacks = do
  sharedItems <- traverse sharedItem rucksacks
  priorities <- traverse priority sharedItems
  pure $ sum (fmap fromIntegral priorities)

main :: IO ()
main = run $ BasicPuzzle parse solve
