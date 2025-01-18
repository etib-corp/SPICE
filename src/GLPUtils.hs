module GLPUtils where

import MyParser
import Structures

import Control.Applicative

import Data.Functor

-- Unquote a string. ("toto" -> toto)
unquote :: String -> String
unquote [] = ""
unquote str = init $ tail str

-- Used to parse multiple kind of strings.
parseConfigString :: Parser String
parseConfigString = parseString <|> parseString'

-- Parse multiple strings until a given string is found.
loopedParser :: [String] -> Parser String
loopedParser [] = fail "Can not parse any elements of given table."
loopedParser (x:xs) = parseGivenString x <|> loopedParser xs

-- Parse a table of strings and returns it in a raw string. ("[toto, tata] yes no" -> "[toto, tata]")
parseTable :: Parser String
parseTable = parseGivenString "[" *> parseSomeUntil (parseAnyChar) (parseChar ']')

-- Parse a table of strings and returns it in a list of strings. ("toto, tata" -> ["toto", "tata"])
parseTable' :: String -> Either Error [String]
parseTable' tab = parse tab (parseSepBy parseStringInQuotes (parseGivenString "," *> parseWhiteSpaces) <* parseWhiteSpaces)

