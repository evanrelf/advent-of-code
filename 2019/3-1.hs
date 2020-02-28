#!/usr/bin/env nix-shell
#!nix-shell --pure --packages ghc -i runghc


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


type Path = [Movement]


type Line = [Point]


--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------


parseMovement :: String -> Movement
parseMovement (direction : distance) = read ([direction] <> " " <> distance)


-- Movements separated by commas
-- Lines separated by newline
parse :: String -> (Path, Path)
parse = undefined


--------------------------------------------------------------------------------
-- Solving
--------------------------------------------------------------------------------


move :: Movement -> Point -> Point
move movement (x, y) =
  case movement of
    U n -> (x, y + n)
    D n -> (x, y - n)
    L n -> (x - n, y)
    R n -> (x + n, y)


-- Maybe something like a scan could collect all the places a point has been,
-- forming the line?
pathToLine :: Path -> Line
pathToLine = undefined


findIntersections :: (Path, Path) -> [Point]
findIntersections = undefined


findClosestIntersection :: [Point] -> Point
findClosestIntersection = undefined


solve :: (Path, Path) -> Int
solve = undefined


--------------------------------------------------------------------------------
-- Running
--------------------------------------------------------------------------------


main :: IO ()
main = print . solve . parse =<< getContents
