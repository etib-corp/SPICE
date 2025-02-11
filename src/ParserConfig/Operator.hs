module ParserConfig.Operator where

import MyParser
import Structures
import Formatters
import LispParser

import Control.Applicative

import Data.Functor

-- Util function to format the operator into a backend format.
formatOperator :: String -> String
formatOperator "plus:" = "+"
formatOperator "minus:" = "-"
formatOperator "multiply:" = "*"
formatOperator "divide:" = "div"
formatOperator "modulo:" = "mod"
formatOperator "equal:" = "eq?"
formatOperator "assignation:" = "define"
formatOperator "greater:" = ">"
formatOperator "less:" = "<"
