module VariableConfig where

import MyParser
import Structures
import Formatters
import GLPUtils

import Control.Applicative

import Data.Functor

-- Create a parser for the variable using the given declarators.
createVariableConfig' :: [String] -> Parser Expr
createVariableConfig' [] = fail "Invalid `variable` configuration."
createVariableConfig' tab = fmap Var (loopedParser tab *> parseWhiteSpaces *> parseName)

-- Create a parser for the variable configuration using the given content and formatters.
createVariableConfig :: [String] -> Formatter -> Parser Expr
createVariableConfig [] _ = fail "Invalid `variable` configuration."
createVariableConfig content (p,s) = parseGivenString p *> createVariableConfig' content <* parseGivenString s

-- Parse a variable configuration and returns it as a tuple containing the formatters and the differents allowed declarators.
parseVariableConfig :: Parser (Formatter, [String])
parseVariableConfig = (,) <$> formatters <*> table
    where
        formatters = parseGivenString "variable" *> parseFormatters <* parseWhiteSpaces
        table = parseGivenString "[" *> parseSepBy (parseStringInQuotes) (parseWhiteSpaces *>
            parseGivenString "," <* parseWhiteSpaces) <* parseGivenString "]" <* parseWhiteSpaces
