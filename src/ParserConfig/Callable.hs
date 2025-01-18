module CallableConfig where

import MyParser
import Structures
import Formatters

import Control.Applicative

import Data.Functor

parseCallableConfig :: Parser (Formatter, [String], String, [String])
parseCallableConfig = do
    formatters <- parseGivenString "callable" *> parseFormatters <* parseWhiteSpaces
    pref <- parseGivenString "[" *>
        parseSepBy parseStringInQuotes (parseWhiteSpaces *> parseGivenString "," *> parseWhiteSpaces)
        <* parseGivenString "]"
    parseWhiteSpaces *> parseGivenString "->" *> parseWhiteSpaces
    sep <- parseStringInQuotes
    parseWhiteSpaces *> parseGivenString "->" *> parseWhiteSpaces
    suf <- parseGivenString "[" *>
        parseSepBy parseStringInQuotes (parseWhiteSpaces *> parseGivenString "," *> parseWhiteSpaces)
        <* parseGivenString "]"
    pure $ (formatters,pref, sep, suf)
