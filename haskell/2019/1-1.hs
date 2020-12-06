{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Relude.Unsafe (read)

import qualified Data.String as String
import qualified System.IO as IO


requiredFuel :: Int -> Int
requiredFuel mass = floor @Double (fromIntegral mass / 3) - 2


main :: IO ()
main = do
  input <- IO.getContents

  input
    & String.lines
    & fmap read
    & fmap requiredFuel
    & sum
    & print
