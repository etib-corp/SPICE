module FunctionConfig where

import MyParser
import Structures
import Formatters
import GLPUtils

import Control.Applicative

import Data.Functor

-- Util function to get the order for the parser in order to parse a function.
getOrder :: [String] -> [String]
getOrder [] = []
getOrder (('[':_):xs) = "declarator" : getOrder xs
getOrder (x:xs) = x : getOrder xs

-- Util function to get the declarators of a function for the parser.
getDeclarators :: [String] -> [String]
getDeclarators [] = []
getDeclarators (('[':xs):_) = case parseTable' xs of
    Left err -> []
    Right value -> value
