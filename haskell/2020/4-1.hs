module Main (main) where

import Relude.Unsafe (read)

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text.IO as Text.IO
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec


data Passport = Passport
  { birthYear :: Maybe Int
  , issueYear :: Maybe Int
  , expirationYear :: Maybe Int
  , height :: Maybe Text
  , hairColor :: Maybe Text
  , eyeColor :: Maybe Text
  , passportId :: Maybe Int
  , countryId :: Maybe Int
  } deriving Show


type Parser = Megaparsec.Parsec Void Text


fieldParser :: Parser (String, String)
fieldParser = do
  key <- Megaparsec.count 3 Megaparsec.lowerChar
  _ <- Megaparsec.char ':'
  value <- Megaparsec.takeWhile1P Nothing (not . Char.isSpace)
  pure (key, toString value)


passportParser :: Parser Passport
passportParser = do
  fields <- fieldParser `Megaparsec.endBy` Megaparsec.spaceChar

  pure Passport
    { birthYear =
        read <$> List.lookup "byr" fields
    , issueYear =
        read <$> List.lookup "iyr" fields
    , expirationYear =
        read <$> List.lookup "eyr" fields
    , height =
        toText <$> List.lookup "hgt" fields
    , hairColor =
        toText <$> List.lookup "hcl" fields
    , eyeColor =
        toText <$> List.lookup "ecl" fields
    , passportId =
        read <$> List.lookup "pid" fields
    , countryId =
        read <$> List.lookup "cid" fields
    }


batchFileParser :: Parser [Passport]
batchFileParser =
  passportParser `Megaparsec.sepBy` Megaparsec.newline <* Megaparsec.eof


isValid :: Passport -> Bool
isValid passport = all isJust
  [ () <$ birthYear passport
  , () <$ issueYear passport
  , () <$ expirationYear passport
  , () <$ height passport
  , () <$ hairColor passport
  , () <$ eyeColor passport
  , () <$ passportId passport
  -- , () <$ countryId passport
  ]


main :: IO ()
main = do
  input <- Text.IO.getContents

  passports <-
    case Megaparsec.parse batchFileParser "" input of
      Left errorBundle -> die (Megaparsec.errorBundlePretty errorBundle)
      Right passports -> pure passports

  let totalCount = length passports
  let validCount = length (filter isValid passports)

  putStrLn ("Total: " <> show totalCount)
  putStrLn ("Valid: " <> show validCount)
