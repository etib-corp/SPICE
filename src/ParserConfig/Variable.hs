module VariableConfig where

import MyParser
import Structures
import Formatters
import GLPUtils

import Control.Applicative

import Data.Functor

createVariableConfig' :: [String] -> Parser Expr
createVariableConfig' [] = fail "Invalid `variable` configuration."
createVariableConfig' tab = fmap Var (loopedParser tab *> parseWhiteSpaces *> parseName)

createVariableConfig :: [String] -> Formatter -> Parser Expr
createVariableConfig [] _ = fail "Invalid `variable` configuration."
createVariableConfig content (p,s) = parseGivenString p *> createVariableConfig' content <* parseGivenString s

parseVariableConfig :: Parser (Formatter, [String])
parseVariableConfig = do
    formatters <- parseGivenString "variable" *> parseFormatters <* parseWhiteSpaces
    table <- parseGivenString "[" *> parseSepBy (parseStringInQuotes) (parseWhiteSpaces *> parseGivenString "," <* parseWhiteSpaces) <* parseGivenString "]" <* parseWhiteSpaces
    pure $ (formatters,table)
