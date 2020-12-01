module Main (main) where

import qualified Data.Text as Text
import qualified Streamly.Prelude as Streamly


main :: IO ()
main =
  Streamly.repeatM getLine
    & Streamly.filter (not . Text.null)
    & Streamly.map Text.reverse
    & Streamly.trace putTextLn
    & Streamly.drain
