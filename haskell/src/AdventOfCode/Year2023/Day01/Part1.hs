{-# LANGUAGE QuasiQuotes #-}

module AdventOfCode.Year2023.Day01.Part1 (solve, tests) where

import Data.Char (isDigit)
import Data.String.Interpolate (__i)
import Data.Text qualified as Text
import Relude.Extra.Bifunctor (bimapBoth)
import Relude.Unsafe (read)
import Test.Tasty
import Test.Tasty.HUnit

solve :: Text -> Word
solve input =
  input
     &  Text.lines
    <&> Text.filter isDigit
    <&> Text.head &&& Text.last
    <&> bimapBoth (one >>> read @Word)
    <&> first (* 10)
    <&> uncurry (+)
     &  sum

tests :: [TestTree]
tests =
  [ testCase "example input" $ solve exampleInput @?= 142
  ]

exampleInput :: Text
exampleInput = [__i|
  1abc2
  pqr3stu8vwx
  a1b2c3d4e5f
  treb7uchet
|]
