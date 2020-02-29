#!/usr/bin/env nix-shell
#!nix-shell --pure --packages ghc -i runghc


{-# LANGUAGE LambdaCase #-}


import Data.Bifunctor (bimap)
import Data.List (delete, intersect, nub)


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
splitOn delimiter = reverse . (\(xs, xss) -> reverse xs : xss) . foldl f ([], []) where
  f (xs, xss) x =
    if x == delimiter
      then ([], reverse xs : xss)
      else (x : xs, xss)


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
  x <- if x1 < x2 then [x1 .. x2] else [x2 .. x1]
  y <- if y1 < y2 then [y1 .. y2] else [y2 .. y1]
  pure (x, y)


verticesToLine :: [Point] -> [Point]
verticesToLine [] = []
verticesToLine [p1] = [p1]
verticesToLine (p1 : p2 : ps) =
  nub (interpolate p1 p2 <> verticesToLine (p2 : ps))


findIntersections :: [Point] -> [Point] -> [Point]
findIntersections xs ys = delete (0, 0) (intersect (nub xs) (nub ys))


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
