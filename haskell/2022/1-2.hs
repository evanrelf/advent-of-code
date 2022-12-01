module Main (main) where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Relude.Unsafe as Unsafe

main :: IO ()
main = do
  input <- Text.getContents
  print $ solve (parse input)

parse :: Text -> [[Word]]
parse input =
  input
  & Text.strip
  & Text.splitOn "\n\n"
  & fmap Text.lines
  & fmap (fmap (toString >>> Unsafe.read @Word))

solve :: [[Word]] -> Word
solve inventories =
  inventories
  & fmap sum
  & sortOn Down
  & take 3
  & sum
