{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE QuasiQuotes #-}

module AdventOfCode (main) where

import Data.String.Interpolate (i)
import Data.Text.IO qualified as Text
import Options.Applicative

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

main :: IO ()
main = do
  options <- getOptions
  case (options.year, options.day, options.part) of
    (y, d, p) -> do
      Text.hPutStrLn stderr [i|No solution for year #{y} day #{d} part #{p}|]
      exitFailure
