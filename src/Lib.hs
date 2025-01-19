-- | This module is a utils library that provide many different type of functions.
-- These functions can help us for the development on multiple kind of task.
module Lib
    ( getInt
    , getDouble
    , isWhiteSpace
    , wLast
    , lastChar
    , catFile
    , getUntilBackspace
    , getUntilChar
    , mapFilterByIndex
    , bytesToString
    , convertToWord
    ) where

import Data.Text (pack)
import Data.Text.Read (double)
import Data.Word (Word8)

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

-- | Returns a list without the last element.
wLast :: [a] -> [a]
wLast [] = []
wLast [_] = []
wLast (x:xs) = x : wLast xs

-- | Returns the last char of a string.
lastChar :: String -> Char
lastChar [] = '\0'
lastChar (x:[]) = x
lastChar (_:xs) = lastChar xs

-- | Print the content of a file.
catFile :: String -> IO ()
catFile path = do
    (readFile path) >>= putStrLn

-- | Returns a string until a given char is found.
getUntilChar :: String -> Char -> String
getUntilChar [] _ = []
getUntilChar (x:xs) c   | x == c = []
                        | otherwise = x : getUntilChar xs c

-- | Returns a string until a backspace is found.
getUntilBackspace :: String -> String
getUntilBackspace [] = []
getUntilBackspace (x:xs)    | x == '\n' = []
                            | otherwise = x : getUntilBackspace xs

-- | Returns the first element of a list that match the given function.
mapFilterByIndex :: [(a, b)] -> (a -> Bool) -> (a, b)
mapFilterByIndex (x:[]) _ = x
mapFilterByIndex ((k,v):xs) func    | func k = (k,v)
                                    | otherwise = mapFilterByIndex xs func

bytesToString :: [Word8] -> String
bytesToString = map (toEnum . fromEnum)

convertToWord :: String -> [Word8]
convertToWord [] = []
convertToWord (x:xs) = (fromIntegral (fromEnum x)) : convertToWord xs
