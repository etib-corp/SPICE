module ConditionConfig where

import MyParser
import Structures
import LispParser
import Formatters
import GLPUtils

import Control.Applicative

import Data.Functor

parseConditionConfig :: Parser Formatter
parseConditionConfig = parseGivenString "condition" *> parseFormatters <*
    (parseWhiteSpaces *> parseGivenString "expression") <|> fail "Invalid `condition` configuration."
