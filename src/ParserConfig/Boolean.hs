module BooleanConfig where

import MyParser
import Structures
import Formatters

import Control.Applicative

import Data.Functor

-- Create a parser for boolean values using the given valid strings and the formatters.
createBooleanParser :: [String] -> Formatter -> Parser Expr
createBooleanParser (x:y:[]) (p,s) = fmap Integer (parseGivenString p *> (parseGivenString x $> 1 <|> parseGivenString y $> 0) <* parseGivenString s)
createBooleanParser _ _ = fail "Invalid boolean configuration: expected two values."

-- Parse the configuration for boolean values.
parseBooleanConfig :: Parser (Parser Expr)
parseBooleanConfig = do
    formatter <- parseGivenString "boolean" *> parseFormatters
    parseWhiteSpaces *> parseChar '[' *> parseWhiteSpaces
    boolValues <- parseSepBy parseStringInQuotes (parseGivenString "," *> parseWhiteSpaces)
    parseChar ']' *> parseWhiteSpaces
    pure (createBooleanParser boolValues formatter)
