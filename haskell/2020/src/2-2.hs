{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Exception (throwIO)
import Relude.Unsafe (read, (!!))

import qualified Data.Text.IO as Text.IO
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec


data Policy = Policy
  { beginning :: Int
  , ending :: Int
  , letter :: Char
  }


parse :: Text -> IO (Policy, String)
parse input = either throwIO pure $ Megaparsec.parse @Void parser "" input
  where
  parser = do
    beginning <- read <$> some Megaparsec.digitChar
    _ <- Megaparsec.char '-'
    ending <- read <$> some Megaparsec.digitChar
    Megaparsec.space1
    letter <- Megaparsec.asciiChar
    _ <- Megaparsec.char ':'
    Megaparsec.space1
    password <- some Megaparsec.asciiChar

    pure (Policy{beginning, ending, letter}, password)


isValid :: (Policy, String) -> Bool
isValid (Policy{beginning, ending, letter}, password) =
  let
    b = password !! (beginning - 1)
    e = password !! (ending - 1)
  in
    (b == letter) `xor` (e == letter)


main :: IO ()
main = do
  input <- Text.IO.getContents

  count <-
    Streamly.fromList (lines input)
      & Streamly.serially
      & Streamly.mapM parse
      & Streamly.filter isValid
      & Streamly.length

  print count
