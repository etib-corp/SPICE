module ConditionConfig where

import MyParser
import Structures
import LispParser
import Formatters
import GLPUtils

import Control.Applicative

import Data.Functor

import Debug.Trace

parseConditionConfig :: Parser (Parser Expr)
parseConditionConfig = do
    formatters <- parseGivenString "condition" *> parseFormatters
    (parseWhiteSpaces *> parseGivenString "expression") <|> fail "Invalid `condition` configuration."
    pure $ (trace "conditoion" parseExpression)
