module Main (main) where

import Relude.Unsafe (read)
import Prelude hiding (max, min)

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
  , passportId :: Maybe Text
  , countryId :: Maybe Int
  } deriving stock Show


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
        toText <$> List.lookup "pid" fields
    , countryId =
        read <$> List.lookup "cid" fields
    }


batchFileParser :: Parser [Passport]
batchFileParser =
  passportParser `Megaparsec.sepBy` Megaparsec.newline <* Megaparsec.eof


isValid :: Passport -> Bool
isValid passport =
  and
    [ birthYear ? inRange 1920 2002
    , issueYear ? inRange 2010 2020
    , expirationYear ? inRange 2020 2030
    , height ? isValidHeight
    , hairColor ? isValidColor
    , eyeColor ? isValidEyeColor
    , passportId ? isValidPassportId
    -- , Validation (countryId passport) (const True)
    ]
  where
  field ? predicate =
    any predicate (field passport)

  inRange min max n =
    n >= min && n <= max

  isValidHeight input =
    case Megaparsec.parse heightParser "" input of
      Left _ -> False
      Right (number, units) ->
        case units of
          "cm" -> inRange 150 193 number
          "in" -> inRange 59 76 number
          _ -> error "unreachable after parsing"

  heightParser :: Parser (Int, Text)
  heightParser = do
    number <- read <$> Megaparsec.some Megaparsec.digitChar
    units <- toText <$> (Megaparsec.string "cm" <|> Megaparsec.string "in")
    pure (number, units)

  isValidColor input =
    case toString input of
      ('#' : cs) | length (filter isHexChar cs) == 6 -> True
      _ -> False

  isHexChar c =
    c `elem` (['0' .. '9'] <> ['a' .. 'f'])

  isValidEyeColor input =
    input `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

  isValidPassportId =
    (== 9) . length . filter isNumberChar . toString

  isNumberChar c =
    c `elem` ['0' .. '9']


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
