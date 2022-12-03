module AdventOfCode.Year2022.Day2.Part1 (main) where

import Relude hiding (round)

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

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

parseShape :: Text -> Shape
parseShape text
  | text `elem` ["A", "X"] = Rock
  | text `elem` ["B", "Y"] = Paper
  | text `elem` ["C", "Z"] = Scissors
  | otherwise = error $ "Unexpected shape code '" <> text <> "'"

parseRound :: Text -> Round
parseRound text = case Text.splitOn " " text of
  [l, r] -> (parseShape l, parseShape r)
  _ -> error $ "Unexpected round '" <> text <> "'"

parse :: Text -> [Round]
parse input = fmap parseRound (lines input)

solve :: [Round] -> Int
solve rounds = sum (fmap scoreRound rounds)

main :: IO ()
main = print . solve . parse =<< Text.getContents
