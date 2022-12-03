module Main (main) where

import Control.Monad.ST (runST)
import Data.STRef
import Relude.Unsafe ((!!))

import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

type Coordinate = (Int, Int, Int)

type PocketDimension = HashSet Coordinate

main :: IO ()
main = do
  input <- Text.getContents
  print $ parse input

parse :: Text -> PocketDimension
parse input =
  input
  & Text.lines
  & fmap toString
  & fmap (fmap \case
      '.' -> False
      '#' -> True
      c -> error ("Unexpected character " <> show c)
    )
  & \grid -> runST do
      ref <- newSTRef HashSet.empty
      let ys = [0 .. length grid - 1]
      let xs = [0 .. length (grid !! 0) - 1]
      for_ ys \y ->
        for_ xs \x ->
          when (grid !! y !! x) do
            modifySTRef ref $ HashSet.insert (x, y, 0)
      readSTRef ref

solve :: PocketDimension -> Word
solve pocketDimension =
  undefined

cycle :: PocketDimension -> PocketDimension
cycle pocketDimension =
  undefined

isActive :: PocketDimension -> Coordinate -> Bool
isActive =
  flip HashSet.member

neighbors :: Coordinate -> [Coordinate]
neighbors c@(x, y, z) = do
  let range n = [n - 1 .. n + 1]
  x' <- range x
  y' <- range y
  z' <- range z
  let c' = (x', y', z')
  guard (c /= c')
  pure c'
