module Main (main) where

import Prelude hiding (map)

import qualified Data.String as String
import qualified System.IO as IO


data Square = Open | Tree


decodeLine :: String -> [Square]
decodeLine = \case
  [] -> []
  ('.' : xs) -> Open : decodeLine xs
  ('#' : xs) -> Tree : decodeLine xs
  (x : _) -> error ("Invalid map square: " <> show x)


checkPosition :: Int -> Int -> [[Square]] -> Maybe Square
checkPosition x y map = map !!? y >>= (!!? x)


traverseMap :: [[Square]] -> Int
traverseMap map = go 0 0 0
  where
  go x y count =
    case checkPosition (x + 3) (y + 1) map of
      Just Tree -> go (x + 3) (y + 1) (count + 1)
      Just Open -> go (x + 3) (y + 1) count
      Nothing -> count


main :: IO ()
main = do
  input <- IO.getContents

  let map = fmap (cycle . decodeLine) (String.lines input)

  print (traverseMap map)
