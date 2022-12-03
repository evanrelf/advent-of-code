module AdventOfCode.Year2022.Day2.Part2 (main) where

import AdventOfCode.Core
import Relude hiding (round)

import qualified Data.Text as Text

data Shape = Rock | Paper | Scissors
  deriving stock (Eq, Enum, Bounded)

data Outcome = Loss | Draw | Win
  deriving stock (Enum)

type Round = (Shape, Outcome)

play :: Shape -> Outcome -> Shape
play l r = case (l, r) of
  (shape, Loss) -> if shape /= minBound then pred shape else maxBound
  (shape, Draw) -> shape
  (shape, Win) -> if shape /= maxBound then succ shape else minBound

scoreShape :: Shape -> Int
scoreShape shape = fromEnum shape + 1

scoreOutcome :: Outcome -> Int
scoreOutcome outcome = fromEnum outcome * 3

scoreRound :: Round -> Int
scoreRound round@(_, outcome) = scoreShape shape + scoreOutcome outcome
  where shape = uncurry play round

parseShape :: Text -> Either Text Shape
parseShape = \case
  "A" -> Right Rock
  "B" -> Right Paper
  "C" -> Right Scissors
  text -> Left $ "Unexpected shape code '" <> text <> "'"

parseOutcome :: Text -> Either Text Outcome
parseOutcome = \case
  "X" -> Right Loss
  "Y" -> Right Draw
  "Z" -> Right Win
  text -> Left $ "Unexpected outcome code '" <> text <> "'"

parseRound :: Text -> Either Text Round
parseRound text = case Text.splitOn " " text of
  [shape, outcome] -> (,) <$> parseShape shape <*> parseOutcome outcome
  _ -> Left $ "Unexpected round '" <> text <> "'"

parse :: Text -> Either Text [Round]
parse input = traverse parseRound (lines input)

solve :: [Round] -> Either Text Int
solve rounds = Right $ sum (fmap scoreRound rounds)

main :: IO ()
main = runPuzzleIO (Puzzle parse solve)
