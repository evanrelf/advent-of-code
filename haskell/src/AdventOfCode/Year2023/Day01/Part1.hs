{-# LANGUAGE QuasiQuotes #-}

module AdventOfCode.Year2023.Day01.Part1 (solve, tests) where

import Data.String.Interpolate (__i)
import Test.Tasty
import Test.Tasty.HUnit

solve :: Text -> Either String Word
solve _ = Left "todo"

tests :: [TestTree]
tests =
  [ testCase "example input" $ solve exampleInput @?= Right 142
  ]

exampleInput :: Text
exampleInput = [__i|
  1abc2
  pqr3stu8vwx
  a1b2c3d4e5f
  treb7uchet
|]
