{-# LANGUAGE QuasiQuotes #-}

module AdventOfCode.Year2023.Day01 (part1, part2, tests) where

import Data.Char (isDigit)
import Data.String.Interpolate (__i)
import Data.Text qualified as Text
import Relude.Extra.Bifunctor (bimapBoth)
import Relude.Unsafe (read)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec
import Text.Megaparsec.Char

part1 :: Text -> Word
part1 input =
  input
     &  Text.lines
    <&> Text.filter isDigit
    <&> Text.head &&& Text.last
    <&> bimapBoth (one >>> read @Word)
    <&> first (* 10)
    <&> uncurry (+)
     &  sum

part2 :: Text -> Word
part2 = undefined

type Parser = Parsec Void Text

digit :: Parser Word
digit =
  asum
    [ digitChar <&> \c -> read [c]
    , "one" $> 1
    , "two" $> 2
    , "three" $> 3
    , "four" $> 4
    , "five" $> 5
    , "six" $> 6
    , "seven" $> 7
    , "eight" $> 8
    , "nine" $> 9
    ]

tests :: [TestTree]
tests =
  [ testCase "part 1" do
      let input = [__i|
            1abc2
            pqr3stu8vwx
            a1b2c3d4e5f
            treb7uchet
          |]
      part1 input @?= 142
  , testCase "part 2" do
      let input = [__i|
            two1nine
            eightwothree
            abcone2threexyz
            xtwone3four
            4nineeightseven2
            zoneight234
            7pqrstsixteen
          |]
      part2 input @?= 281
  ]
