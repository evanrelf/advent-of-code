module AdventOfCode.Year2022.Day2.Part1 (main) where

import AdventOfCode.Core
import Relude hiding (round)

import qualified Data.Text as Text

data Shape = Rock | Paper | Scissors
  deriving stock (Eq, Enum)

data Outcome = Loss | Draw | Win
  deriving stock (Enum)

type Round = (Shape, Shape)

play :: Shape -> Shape -> Outcome
play l r = case (l, r) of
  (Rock, Paper) -> Win
  (Paper, Scissors) -> Win
  (Scissors, Rock) -> Win
  _ | l == r -> Draw
  _ -> Loss

scoreShape :: Shape -> Int
scoreShape shape = fromEnum shape + 1

scoreOutcome :: Outcome -> Int
scoreOutcome outcome = fromEnum outcome * 3

scoreRound :: Round -> Int
scoreRound round@(_, shape) = scoreShape shape + scoreOutcome outcome
  where outcome = uncurry play round

parseShape :: Text -> Either Text Shape
parseShape text
  | text `elem` ["A", "X"] = Right Rock
  | text `elem` ["B", "Y"] = Right Paper
  | text `elem` ["C", "Z"] = Right Scissors
  | otherwise = Left $ "Unexpected shape code '" <> text <> "'"

parseRound :: Text -> Either Text Round
parseRound text = case Text.splitOn " " text of
  [l, r] -> (,) <$> parseShape l <*> parseShape r
  _ -> Left $ "Unexpected round '" <> text <> "'"

parse :: Text -> Either Text [Round]
parse input = traverse parseRound (lines input)

solve :: [Round] -> Either Text Int
solve rounds = Right $ sum (fmap scoreRound rounds)

main :: IO ()
main = runPuzzleIO (Puzzle parse solve)
