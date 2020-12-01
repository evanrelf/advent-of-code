{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Data.String.Interpolate (__i)
import Data.Vector (Vector)
import Relude.Unsafe (read)
import System.IO.Error (IOError)

import qualified Data.Vector as Vector
import qualified Streamly
import qualified Streamly.Prelude as Streamly


main :: IO ()
main =
  readEntries >>= bruteForce >>= \case
    Nothing ->
      putStrLn "Couldn't find solution"

    Just (fromEnum -> x, fromEnum -> y) ->
      putStrLn [__i|
        Found match: #{x} + #{y} = #{x + y}
        Solution:    #{x} * #{y} = #{x * y}
      |]


readEntries :: IO (Vector Word16)
readEntries
  = Streamly.repeatM getLine
  & Streamly.handle (\(_ :: IOError) -> Streamly.nil)
  & Streamly.map (read . toString)
  & Streamly.foldl' Vector.snoc Vector.empty


bruteForce :: Vector Word16 -> IO (Maybe (Word16, Word16))
bruteForce entries
  = Streamly.parallely do
      x <- Streamly.fromFoldable entries
      y <- Streamly.fromFoldable entries
      Streamly.yield (x, y)
  & Streamly.find (\(x, y) -> x + y == 2020)
