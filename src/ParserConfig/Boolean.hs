module BooleanConfig where

import MyParser
import Structures
import Formatters

import Control.Applicative

import Data.Functor

createBooleanParser :: [String] -> Formatter -> Parser Expr
createBooleanParser (x:y:[]) (p,s) = fmap Integer (parseGivenString p *> (parseGivenString x $> 1 <|> parseGivenString y $> 0) <* parseGivenString s)
createBooleanParser _ _ = fail "Invalid boolean configuration: expected two values."

parseBooleanConfig :: Parser (Parser Expr)
parseBooleanConfig = do
    formatter <- parseGivenString "boolean" *> parseFormatters
    parseWhiteSpaces *> parseChar '[' *> parseWhiteSpaces
    boolValues <- parseSepBy parseStringInQuotes (parseGivenString "," *> parseWhiteSpaces)
    parseChar ']' *> parseWhiteSpaces
    pure (createBooleanParser boolValues formatter)
