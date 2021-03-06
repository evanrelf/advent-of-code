{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}


import Control.Category ((>>>))
import Data.Bifunctor (bimap)
import Data.List (delete)
import Data.Maybe (catMaybes)
import Debug.Trace


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
  = Horizontal
  | Vertical
  deriving (Eq, Show)


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


-- pointOnLineSegment :: LineSegment -> Point -> Bool
-- pointOnLineSegment ((px, py), (qx, qy)) (rx, ry) =
--   if px /= qx && py /= qy then
--     error "pointOnLineSegment doesn't work with diagonal line segments"
--   else
--     and
--       [ rx >= min px qx
--       , rx <= max px qx
--       , ry >= min py qy
--       , ry <= max py qy
--       ]


orientation :: LineSegment -> Orientation
orientation ((px, py), (qx, qy)) =
  if | px == qx && py /= qy -> Vertical
     | px /= qx && py == qy -> Horizontal
     | otherwise ->
         error "pointOnLineSegment doesn't work with diagonal line segments"

-- Credit to http://cs.swan.ac.uk/~cssimon/line_intersection.html
intersection :: LineSegment -> LineSegment -> Maybe Point
intersection l1 l2 | l1 == l2 =
  error "intersection doesn't work with identical lines"
intersection l1@((x1, y1), (x2, y2)) l2@((x3, y3), (x4, y4)) =
  let
    ta :: Double
    ta = fromIntegral (((y3 - y4) * (x1 - x3)) + ((x4 - x3) * (y1 - y3)))
       / fromIntegral (((x4 - x3) * (y1 - y2)) - ((x1 - x2) * (y4 - y3)))
    tb :: Double
    tb = fromIntegral (((y1 - y2) * (x1 - x3)) + ((x2 - x1) * (y1 - y3)))
       / fromIntegral (((x4 - x3) * (y1 - y2)) - ((x1 - x2) * (y4 - y3)))
  in
    if (0 <= ta && ta <= 1) && (0 <= tb && tb <= 1) then
      Just undefined
    else
      Nothing


  -- case (orientation l1, orientation l2) of
  --   (Horizontal, Horizontal) -> Nothing
  --   (Vertical, Vertical) -> Nothing
  --   (Horizontal, Vertical) ->
  --     let
  --       y = p1y
  --       x = p2x
  --       hBegin = min p1x q1x
  --       hEnd = max p1x q1x
  --       vBegin = min p2y q2y
  --       vEnd = max p2y q2y
  --     in
  --       if (x >= hBegin && x <= hEnd) || (y >= vBegin && y <= vEnd) then
  --         traceShow ((y, hBegin, hEnd), (x, vBegin, vEnd)) $ traceShowId $ Just (x, y)
  --       else
  --         Nothing
  --   (Vertical, Horizontal) ->
  --     let
  --       y = p2y
  --       x = p1x
  --       hBegin = min p2x q2x
  --       hEnd = max p2x q2x
  --       vBegin = min p1y q1y
  --       vEnd = max p1y q1y
  --     in
  --       if (x >= hBegin && x <= hEnd) || (y >= vBegin && y <= vEnd) then
  --         traceShow ((y, hBegin, hEnd), (x, vBegin, vEnd)) $ traceShowId $ Just (x, y)
  --       else
  --         Nothing


intersections :: [LineSegment] -> [LineSegment] -> [Point]
intersections [] _ = []
intersections _ [] = []
intersections l1s l2s = catMaybes [intersection l1 l2 | l1 <- l1s, l2 <- l2s]


closest :: [Point] -> Int
closest = traceShowId . minimum . fmap (\(x, y) -> abs x + abs y) . delete (0, 0)


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
