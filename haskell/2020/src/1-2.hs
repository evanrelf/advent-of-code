module Main (main) where

import Relude.Unsafe (read)

import qualified Data.String as String
import qualified System.IO as IO


main :: IO ()
main = do
  input <- IO.getContents

  let entries :: [Integer]
      entries = fmap read . String.lines $ input

  let bruteForce = do
        x <- entries
        y <- entries
        z <- entries
        guard (x + y + z == 2020)
        pure (x * y * z)

  case bruteForce of
    (x : _) -> print x
    [] -> die "Couldn't find solution"
