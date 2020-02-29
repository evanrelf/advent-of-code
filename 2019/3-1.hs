#!/usr/bin/env nix-shell
#!nix-shell --pure --packages ghc -i runghc


{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}


import Control.Category ((>>>))
import Data.Bifunctor (bimap)
import Data.Maybe (catMaybes)


both :: (a -> b) -> (a, a) -> (b, b)
both f = bimap f f


--------------------------------------------------------------------------------
-- Modeling
--------------------------------------------------------------------------------


data Movement
  = U Int
  | D Int
  | L Int
  | R Int
  deriving (Show, Read)


type Point = (Int, Int)


type LineSegment = (Point, Point)


data Orientation
  = Clockwise
  | Counterclockwise
  | Collinear
  deriving Show


--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------


parseMovement :: String -> Movement
parseMovement [] = error "parseMovement doesn't work with an empty string"
parseMovement (direction : distance) = read ([direction] <> " " <> distance)


splitOn :: Eq a => a -> [a] -> [[a]]
splitOn delimiter
  = reverse
  . (\(xs, xss) -> reverse xs : xss)
  . foldl f ([], [])
  where
    f (xs, xss) x =
      if x == delimiter
        then ([], reverse xs : xss)
        else (x : xs, xss)


parse :: String -> ([Movement], [Movement])
parse = lines >>> \case
  (l1 : l2 : []) -> both (fmap parseMovement . splitOn ',') (l1, l2)
  _ -> error "parse expects 2 lines"


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


verticesToLineSegments :: [Point] -> [LineSegment]
verticesToLineSegments (v1 : v2 : vs) =
  (v1, v2) : verticesToLineSegments (v2 : vs)
verticesToLineSegments _ = []


pointOnLineSegment :: LineSegment -> Point -> Bool
pointOnLineSegment ((px, py), (qx, qy)) (rx, ry) =
  if px /= qx && py /= qy then
    error "pointOnLineSegment doesn't work with diagonal line segments"
  else
    and
      [ rx >= min px qx
      , rx <= max px qx
      , ry >= min py qy
      , ry <= max py qy
      ]


intersection :: LineSegment -> LineSegment -> Maybe Point
intersection l1 l2 | l1 == l2 =
  error "intersection doesn't work with identical lines"
intersection (p1, q1) (p2, q2) = undefined


intersections :: [LineSegment] -> [LineSegment] -> [Point]
intersections [] _ = []
intersections _ [] = []
intersections l1s l2s = catMaybes [intersection l1 l2 | l1 <- l1s, l2 <- l2s]


closest :: [Point] -> Int
closest = minimum . fmap (\(x, y) -> abs x + abs y)


solve :: ([Movement], [Movement]) -> Int
solve
  = closest
  . uncurry intersections
  . both verticesToLineSegments
  . both movementsToVertices


--------------------------------------------------------------------------------
-- Running
--------------------------------------------------------------------------------


main :: IO ()
main = print . solve . parse =<< getContents
