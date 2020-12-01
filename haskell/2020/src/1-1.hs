{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Data.String.Interpolate (__i)
import Relude.Unsafe (read)
import System.IO.Error (IOError)

import qualified Streamly
import qualified Streamly.Prelude as Streamly


main :: IO ()
main =
  readEntries >>= bruteForce >>= \case
    Nothing ->
      putStrLn "Couldn't find solution"

    Just (toInteger -> x, toInteger -> y) ->
      putStrLn [__i|
        Found match: #{x} + #{y} = #{x + y}
        Solution:    #{x} * #{y} = #{x * y}
      |]


readEntries :: IO [Word16]
readEntries
  = Streamly.repeatM getLine
  & Streamly.handle (\(_ :: IOError) -> Streamly.nil)
  & Streamly.map (read . toString)
  & Streamly.toList
  & fmap sort


bruteForce :: [Word16] -> IO (Maybe (Word16, Word16))
bruteForce entries
  = Streamly.serially do
      x <- Streamly.fromList entries
      y <- Streamly.fromList entries
      Streamly.yield (x, y)
  & Streamly.find (\(x, y) -> x + y == 2020)
