module GLPUtils where

import MyParser
import Structures

import Control.Applicative

import Data.Functor

unquote :: String -> String
unquote [] = ""
unquote str = init $ tail str

parseConfigString :: Parser String
parseConfigString = parseString <|> parseString'

loopedParser :: [String] -> Parser String
loopedParser [] = fail "Can not parse any elements of given table."
loopedParser (x:xs) = parseGivenString x <|> loopedParser xs

parseTable :: Parser String
parseTable = parseGivenString "[" *> parseSomeUntil (parseAnyChar) (parseChar ']')

parseTable' :: String -> Either Error [String]
parseTable' tab = parse tab (parseSepBy parseStringInQuotes (parseGivenString "," *> parseWhiteSpaces) <* parseWhiteSpaces)

