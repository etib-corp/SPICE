module ParserConfig.Condition where

import MyParser
import Structures
import LispParser
import Formatters
import GLPUtils

import Control.Applicative

import Data.Functor

-- Parse the configuration for a condition and returns a tuple containing the formatters.
parseConditionConfig :: Parser Formatter
parseConditionConfig = parseGivenString "condition" *> parseFormatters <*
    (parseWhiteSpaces *> parseGivenString "expression") <|> fail "Invalid `condition` configuration."
