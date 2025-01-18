module Formatters where

import MyParser

import Control.Applicative

import Data.Functor

type Formatter = (String, String)

parsePrefixConfig :: Parser String
parsePrefixConfig = (parseGivenString "prefix:" *> parseWhiteSpaces *> parseStringInQuotes) <|> parseGivenString ""

parseSuffixConfig :: Parser String
parseSuffixConfig = (parseGivenString "suffix:" *> parseWhiteSpaces *> parseStringInQuotes) <|> parseGivenString ""

parseEmptyFormatter :: Parser Formatter
parseEmptyFormatter = parseGivenString ":" $> ("","")

parseFullFormatters :: Parser Formatter
parseFullFormatters = do
    parseGivenString "{" *> parseWhiteSpaces
    prefix <- parsePrefixConfig
    ((parseWhiteSpaces *> parseGivenString ",") <|> parseGivenString "") *> parseWhiteSpaces
    suffix <- parseSuffixConfig
    parseGivenString "}"
    pure $ (prefix, suffix)

parseFormatters :: Parser Formatter
parseFormatters = (parseFullFormatters <* parseWhiteSpaces <* parseGivenString ":") <|> parseEmptyFormatter
