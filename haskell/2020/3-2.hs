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


traverseMap :: Int -> Int -> [[Square]] -> Int
traverseMap right down map = go 0 0 0
  where
  go x y count =
    case checkPosition (x + right) (y + down) map of
      Just Tree -> go (x + right) (y + down) (count + 1)
      Just Open -> go (x + right) (y + down) count
      Nothing -> count


main :: IO ()
main = do
  input <- IO.getContents

  let map = fmap (cycle . decodeLine) (String.lines input)

  print $ product
    [ traverseMap 1 1 map
    , traverseMap 3 1 map
    , traverseMap 5 1 map
    , traverseMap 7 1 map
    , traverseMap 1 2 map
    ]
