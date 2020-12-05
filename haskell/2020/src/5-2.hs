{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

module Main (main) where

import Control.Lens (over)
import Data.Generics.Labels ()
import Prelude hiding (max, min)

import qualified Data.String as String
import qualified System.IO as IO


data Region = Region
  { rows :: (Int, Int)
  , columns :: (Int, Int)
  } deriving stock Generic


data Seat = Seat
  { row :: Int
  , column :: Int
  } deriving stock Generic


initialRegion :: Region
initialRegion = Region
  { rows = (0, 127)
  , columns = (0, 7)
  }


lower :: (Int, Int) -> (Int, Int)
lower (min, max) = (min, max - half)
  where half = ((max - min) + 1) `div` 2


upper :: (Int, Int) -> (Int, Int)
upper (min, max) = (min + half, max)
  where half = ((max - min) + 1) `div` 2


front :: Region -> Region
front = over #rows lower


back :: Region -> Region
back = over #rows upper


left :: Region -> Region
left = over #columns lower


right :: Region -> Region
right = over #columns upper


decode :: String -> Seat
decode
  = fromMaybe (error "uh oh")
  . toSeat
  . foldl' acc initialRegion
  where
  acc region = \case
    'F' -> front region
    'B' -> back region
    'L' -> left region
    'R' -> right region
    letter -> error ("Unexpected letter: " <> show letter)


toSeat :: Region -> Maybe Seat
toSeat Region{rows, columns} = do
  let (minRow, maxRow) = rows
  let (minColumn, maxColumn) = columns

  if minRow == maxRow && minColumn == maxColumn then
    Just Seat{row = minRow, column = minColumn}
  else
    Nothing


seatId :: Seat -> Int
seatId Seat{row, column} = (row * 8) + column


findMissingSeatId :: [Int] -> Maybe Int
findMissingSeatId (x : y : rest)
  | x /= y - 1 = Just (y - 1)
  | otherwise = findMissingSeatId (y : rest)
findMissingSeatId _ = Nothing


main :: IO ()
main = do
  input <- IO.getContents

  let seatIds =
        input
          & String.lines
          & fmap decode
          & fmap seatId
          & sort

  print (findMissingSeatId seatIds)
