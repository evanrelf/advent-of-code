module AdventOfCode.Year2022.Day2.Part2 (main) where

import Relude hiding (round)

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

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

parseShape :: Text -> Shape
parseShape = \case
  "A" -> Rock
  "B" -> Paper
  "C" -> Scissors
  text -> error $ "Unexpected shape code '" <> text <> "'"

parseOutcome :: Text -> Outcome
parseOutcome = \case
  "X" -> Loss
  "Y" -> Draw
  "Z" -> Win
  text -> error $ "Unexpected outcome code '" <> text <> "'"

parseRound :: Text -> Round
parseRound text = case Text.splitOn " " text of
  [shape, outcome] -> (parseShape shape, parseOutcome outcome)
  _ -> error $ "Unexpected round '" <> text <> "'"

parse :: Text -> [Round]
parse input = fmap parseRound (lines input)

solve :: [Round] -> Int
solve rounds = sum (fmap scoreRound rounds)

main :: IO ()
main = print . solve . parse =<< Text.getContents
