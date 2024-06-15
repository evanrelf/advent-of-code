{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE QuasiQuotes #-}

module AdventOfCode (main, test) where

import AdventOfCode.Year2023.Day01.Part1 qualified
import Control.Exception (throwIO)
import Data.ByteString qualified as ByteString
import Data.String.Interpolate (i)
import Data.Text.IO qualified as Text
import Options.Applicative
import Test.Tasty qualified as Tasty

main :: IO ()
main = do
  options <- getOptions
  solve <- case (options.year, options.day, options.part) of
    (y, d, p) -> do
      Text.hPutStrLn stderr [i|No solution for year #{y} day #{d} part #{p}|]
      exitFailure
  inputBytes <- ByteString.getContents
  input <- either throwIO pure (decodeUtf8' inputBytes)
  case solve input of
    Left err -> die err
    Right (Showable solution) -> print solution

test :: IO ()
test = Tasty.defaultMain . Tasty.testGroup "tests" $
  [ Tasty.testGroup "2023 01 1" AdventOfCode.Year2023.Day01.Part1.tests
  ]

data Options = Options
  { year :: Word16
  , day :: Word8
  , part :: Word8
  }

parseOptions :: Parser Options
parseOptions = do
  year <- option auto (long "year")
  day <- option auto (long "day")
  part <- option auto (long "part")
  pure Options{ year, day, part }

getOptions :: IO Options
getOptions = do
  let parserPrefs = prefs mempty
  let parserInfo = info (helper <*> parseOptions) mempty
  customExecParser parserPrefs parserInfo

data Showable where
  Showable :: Show a => a -> Showable
