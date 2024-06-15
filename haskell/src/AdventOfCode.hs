{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module AdventOfCode (main, test) where

import AdventOfCode.Year2023.Day01 qualified
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
    (2023, 01, 1) -> solution AdventOfCode.Year2023.Day01.part1
    (2023, 01, 2) -> solution AdventOfCode.Year2023.Day01.part1
    (y, d, p) -> do
      Text.hPutStrLn stderr [i|No solution for year #{y} day #{d} part #{p}|]
      exitFailure
  inputBytes <- ByteString.getContents
  input <- either throwIO pure (decodeUtf8' inputBytes)
  either die print (solve input)

test :: IO ()
test = Tasty.defaultMain . Tasty.testGroup "tests" $
  [ Tasty.testGroup "2023 01 1" AdventOfCode.Year2023.Day01.tests
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

class Solution a where
  solution :: Applicative f => (t -> a) -> f (t -> Either String String)

instance {-# OVERLAPPABLE #-} Show a => Solution a where
  solution f = pure (Right . show . f)

instance {-# OVERLAPPABLE #-} Show a => Solution (Maybe a) where
  solution f = pure (Right . show . f)

instance Solution (Maybe String) where
  solution f = pure (maybe (Left "error") Right . f)

instance Solution (Maybe Text) where
  solution f = pure (maybe (Left "error") (Right . toString) . f)

instance {-# OVERLAPPABLE #-} Show a => Solution (Either String a) where
  solution f = pure (fmap show . f)

instance Solution (Either String String) where
  solution f = pure f

instance Solution (Either String Text) where
  solution f = pure (fmap toString . f)
