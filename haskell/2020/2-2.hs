{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Relude.Unsafe (read, (!!))

import qualified Data.String as String
import qualified System.IO as IO


data Policy = Policy
  { beginning :: Int
  , ending :: Int
  , letter :: Char
  }


parse :: String -> (Policy, String)
parse input =
  let
    beginning = read . takeWhile (/= '-') $ input
    ending = read . takeWhile (/= ' ') . drop 1 . dropWhile (/= '-') $ input
    letter = (!! 1) . dropWhile (/= ' ') $ input
    password = reverse . takeWhile (/= ' ') . reverse $ input
  in
    (Policy{beginning, ending, letter}, password)


isValid :: (Policy, String) -> Bool
isValid (Policy{beginning, ending, letter}, password) =
  let
    b = password !! (beginning - 1)
    e = password !! (ending - 1)
  in
    (b == letter) `xor` (e == letter)


main :: IO ()
main = do
  input <- IO.getContents

  input
    & String.lines
    & fmap parse
    & filter isValid
    & length
    & print
