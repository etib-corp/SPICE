module FunctionConfig where

import MyParser
import Structures
import Formatters
import GLPUtils

import Control.Applicative

import Data.Functor

getOrder :: [String] -> [String]
getOrder [] = []
getOrder (('[':_):xs) = "declarator" : getOrder xs
getOrder (x:xs) = x : getOrder xs

getDeclarators :: [String] -> [String]
getDeclarators [] = []
getDeclarators (('[':xs):_) = case parseTable' xs of
    Left err -> []
    Right value -> value
