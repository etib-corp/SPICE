module ParametersConfig where

import MyParser
import Structures
import Formatters
import GLPUtils

import Control.Applicative

import Data.Functor

-- Create a parser for the parameters configuration using the given content and formatters.
createParametersConfig :: [String] -> Formatter -> Parser [String]
createParametersConfig ("name":y:[]) (p,s) = parseGivenString p *> parseWhiteSpaces *>
    (parseSepBy parseName (parseWhiteSpaces *> parseGivenString y *> parseWhiteSpaces) <|> pure []) <*
    parseWhiteSpaces <* parseGivenString s
createParametersConfig ("name":[]) (p,s) = parseGivenString p *> parseWhiteSpaces *>
    (parseSepBy parseName parseWhiteSpaces <|> pure []) <* parseWhiteSpaces <* parseGivenString s
createParametersConfig _ _ = fail "Invalid `parameters` configuration."

-- Parse the configuration for parameters.
parseParametersConfig :: Parser (Parser [String])
parseParametersConfig = do
    formatters <- parseGivenString "parameters" *> parseFormatters
    parseWhiteSpaces
    content <- parseSepBy (parseStringInQuotes <|> parseConfigString) (parseGivenString "->" *> parseWhiteSpaces)
    pure $ (createParametersConfig content formatters)
