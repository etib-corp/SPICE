module CallableConfig where

import MyParser
import Structures
import Formatters

import Control.Applicative

import Data.Functor

-- Parse the configuration for a callable and returns it as a tuple.
parseCallableConfig :: Parser (Formatter, [String], String, [String])
parseCallableConfig = (,,,) <$> formatters <*> pref <*> sep <*> suf
    where
        formatters = parseGivenString "callable" *> parseFormatters <* parseWhiteSpaces
        pref = parseGivenString "[" *>
            parseSepBy parseStringInQuotes (parseWhiteSpaces *> parseGivenString "," *> parseWhiteSpaces) <* parseGivenString "]"
        sep = parseWhiteSpaces *> parseGivenString "->" *> parseWhiteSpaces *> parseStringInQuotes <*
            parseWhiteSpaces <* parseGivenString "->" <* parseWhiteSpaces
        suf = parseGivenString "[" *>
            parseSepBy parseStringInQuotes (parseWhiteSpaces *> parseGivenString "," *> parseWhiteSpaces) <* parseGivenString "]"
