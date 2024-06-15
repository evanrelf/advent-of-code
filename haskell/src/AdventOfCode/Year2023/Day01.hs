{-# LANGUAGE QuasiQuotes #-}

module AdventOfCode.Year2023.Day01 (part1, tests) where

import Data.Char (isDigit)
import Data.String.Interpolate (__i)
import Data.Text qualified as Text
import Relude.Extra.Bifunctor (bimapBoth)
import Relude.Unsafe (read)
import Test.Tasty
import Test.Tasty.HUnit

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
  ]
