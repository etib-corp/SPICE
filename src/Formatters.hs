module Formatters where

import MyParser
import Structures

import Control.Applicative

import Data.Functor

-- Parse a prefix formatter configuration.
parsePrefixConfig :: Parser String
parsePrefixConfig = (parseGivenString "prefix:" *> parseWhiteSpaces *> parseStringInQuotes) <|> parseGivenString ""

-- Parse a suffix formatter configuration.
parseSuffixConfig :: Parser String
parseSuffixConfig = (parseGivenString "suffix:" *> parseWhiteSpaces *> parseStringInQuotes) <|> parseGivenString ""

-- Return an empty formatter. (If there is no configuration for the formatter.)
parseEmptyFormatter :: Parser Formatter
parseEmptyFormatter = parseGivenString ":" $> ("","")

-- Parse the configuration for the formatters. (Including the prefix and suffix.)
parseFullFormatters :: Parser Formatter
parseFullFormatters = do
    parseGivenString "{" *> parseWhiteSpaces
    prefix <- parsePrefixConfig
    ((parseWhiteSpaces *> parseGivenString ",") <|> parseGivenString "") *> parseWhiteSpaces
    suffix <- parseSuffixConfig
    parseGivenString "}"
    pure $ (prefix, suffix)

-- Parse the configuration for the formatters. (Including the prefix and suffix and without them.)
parseFormatters :: Parser Formatter
parseFormatters = (parseFullFormatters <* parseWhiteSpaces <* parseGivenString ":") <|> parseEmptyFormatter
