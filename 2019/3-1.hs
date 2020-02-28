#!/usr/bin/env nix-shell
#!nix-shell --pure --packages ghc -i runghc


{-# LANGUAGE LambdaCase #-}


import Data.Bifunctor (bimap)
import Data.List (union)


both :: (a -> b) -> (a, a) -> (b, b)
both f = bimap f f


--------------------------------------------------------------------------------
-- Modeling
--------------------------------------------------------------------------------


type Point = (Int, Int)


data Movement
  = U Int
  | D Int
  | L Int
  | R Int
  deriving (Show, Read)


--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------


parseMovement :: String -> Movement
parseMovement [] = error "Can't parse movement from empty string"
parseMovement (direction : distance) = read ([direction] <> " " <> distance)


splitOn :: Eq a => a -> [a] -> [[a]]
splitOn delimiter = reverse . snd . foldl f ([], []) where
  f (y, ys) x =
    if x == delimiter
      then ([], reverse y : ys)
      else (x : y, ys)


parse :: String -> ([Movement], [Movement])
parse = (\case (p1 : p2 : _) -> both f (p1, p2); _ -> error "Not 2 lines") . lines where
  f = fmap parseMovement . splitOn ','


--------------------------------------------------------------------------------
-- Solving
--------------------------------------------------------------------------------


move :: Point -> Movement -> Point
move (x, y) = \case
  U n -> (x, y + n)
  D n -> (x, y - n)
  L n -> (x - n, y)
  R n -> (x + n, y)


movementsToVertices :: [Movement] -> [Point]
movementsToVertices = scanl move (0, 0)


interpolate :: Point -> Point -> [Point]
interpolate (x1, y1) (x2, y2) = do
  x <- [x1 .. x2]
  y <- [y1 .. y2]
  pure (x, y)


verticesToLine :: [Point] -> [Point]
verticesToLine [] = []
verticesToLine [p1] = [p1]
verticesToLine (p1 : p2 : ps) = interpolate p1 p2 <> verticesToLine (p2 : ps)


findIntersections :: [Point] -> [Point] -> [Point]
findIntersections = union


findClosestDistance :: [Point] -> Int
findClosestDistance = minimum . fmap (uncurry (+) . both abs)


solve :: ([Movement], [Movement]) -> Int
solve = findClosestDistance
      . uncurry findIntersections
      . both verticesToLine
      . both movementsToVertices


--------------------------------------------------------------------------------
-- Running
--------------------------------------------------------------------------------


main :: IO ()
main = print . solve . parse =<< getContents
