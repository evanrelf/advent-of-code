{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Exception (throwIO)
import Data.Map.Strict (Map)
import Relude.Unsafe (read)
import Prelude hiding (Map)

import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as Text.IO
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec


data Policy = Policy
  { letter :: Char
  , minimum :: Natural
  , maximum :: Natural
  }


type Password = Map Char Natural


parse :: Text -> IO (Policy, String)
parse input = either throwIO pure $ Megaparsec.parse @Void parser "" input
  where
  parser = do
    minimum <- read <$> some Megaparsec.digitChar
    _ <- Megaparsec.char '-'
    maximum <- read <$> some Megaparsec.digitChar
    Megaparsec.space1
    letter <- Megaparsec.asciiChar
    _ <- Megaparsec.char ':'
    Megaparsec.space1
    password <- some Megaparsec.asciiChar

    pure (Policy{letter, minimum, maximum}, password)


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
  input <- Text.IO.getContents

  count <-
    Streamly.fromList (lines input)
      & Streamly.serially
      & Streamly.mapM parse
      & Streamly.map (second countLetters)
      & Streamly.filter isValid
      & Streamly.length

  print count
