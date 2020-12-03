{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Data.Map.Strict (Map)
import Relude.Extra.Bifunctor (secondF)
import Relude.Unsafe (head, read)
import Prelude hiding (Map, head)

import qualified Data.Map.Strict as Map
import qualified Data.String as String
import qualified System.IO as IO


data Policy = Policy
  { minimum :: Natural
  , maximum :: Natural
  , letter :: Char
  } deriving Show


type Password = Map Char Natural


parse :: String -> (Policy, String)
parse input =
  let
    minimum = read . takeWhile (/= '-') $ input
    maximum = read . takeWhile (/= ' ') . drop 1 . dropWhile (/= '-') $ input
    letter = head . drop 1 . dropWhile (/= ' ') $ input
    password = reverse . takeWhile (/= ' ') . reverse $ input
  in
    (Policy{minimum, maximum, letter}, password)


countLetters :: String -> Password
countLetters = foldl' acc Map.empty
  where
  acc password letter = Map.alter inc letter password
  inc = Just . maybe 1 (+ 1)


isValid :: (Policy, Password) -> Bool
isValid (Policy{letter, minimum, maximum}, password) =
  count >= minimum && count <= maximum
  where
  count = fromMaybe 0 (Map.lookup letter password)


main :: IO ()
main = do
  input <- IO.getContents

  input
    & String.lines
    & fmap parse
    & secondF countLetters
    & filter isValid
    & length
    & print
