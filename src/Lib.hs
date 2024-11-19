module Lib
    ( getInt, getDouble, isWhiteSpace
    ) where

import Data.Text (pack)
import Data.Text.Read (double)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

getInt :: String -> Maybe Int
getInt s = case reads s of
    [(n, "")] -> Just n
    _ -> Nothing

getDouble :: String -> Maybe Double
getDouble s = case double $ pack s of
   Right (d, _) -> Just d
   Left _ -> Nothing

isWhiteSpace :: Char -> Bool
isWhiteSpace ' ' = True
isWhiteSpace '\n' = True
isWhiteSpace '\t' = True
isWhiteSpace _ = False
