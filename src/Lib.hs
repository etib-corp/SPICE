-- | This module is a utils library that provide many different type of functions.
-- These functions can help us for the development on multiple kind of task.
module Lib
    ( getInt
    , getDouble
    , isWhiteSpace
    , wLast
    , lastChar
    , catFile
    ) where

import Data.Text (pack)
import Data.Text.Read (double)

-- | Returns an integer extracted from the given string.
getInt :: String -> Maybe Int
getInt s = case reads s of
    [(n, "")] -> Just n
    _ -> Nothing

-- | Returns a double extracted from the given string.
getDouble :: String -> Maybe Double
getDouble s = case double $ pack s of
   Right (d, _) -> Just d
   Left _ -> Nothing

-- | Returns whether the char given is a whitespace or not.
isWhiteSpace :: Char -> Bool
isWhiteSpace ' ' = True
isWhiteSpace '\n' = True
isWhiteSpace '\t' = True
isWhiteSpace _ = False

wLast :: String -> String
wLast [] = []
wLast [x] = []
wLast (x:xs) = x : wLast xs

lastChar :: String -> Char
lastChar [] = '\0'
lastChar (x:[]) = x
lastChar (_:xs) = lastChar xs

catFile :: String -> IO ()
catFile path = do
    (readFile path) >>= putStrLn