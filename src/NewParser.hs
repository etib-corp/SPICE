{-# LANGUAGE RankNTypes #-}

module NewParser (
    Parser(..)
    , parseAny
    , parseChar
    , parseAnyChar
    , parseManyUntil
    , parseString
    , parseWhitespace
    , parseEof
    , parseSatisfy
    , parseSepBy
    , parseOpener
    , parseCloser
    , myMapFirst
    ) where

import Lib

import Control.Applicative
import Control.Monad
import Data.Functor.Identity
import Data.List
import Data.Char
import qualified Data.List
import Data.Functor
import Control.Applicative

data Error = Error { msg :: String, pos :: Int } deriving (Show)
data State = State { str :: String, position :: Int} deriving (Show)

data Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

instance Functor Parser where
    fmap f p = Parser (\s -> fmap (myMapFirst f) (runParser p s))

instance Applicative Parser where
    pure e = Parser (\s -> pure (e,s))
    p1 <*> p2 = Parser (\s -> (\(a,s') -> runParser (fmap a p2) s') =<< (runParser p1 s))

instance Alternative Parser where
    empty = Parser (\_ -> Nothing)
    p1 <|> p2 = Parser (\s -> runParser p1 s <|> runParser p2 s)

instance Monad Parser where
    p >>= f = Parser (\s -> runParser p s >>= (\(a, s') -> runParser (f a) s'))


myMapFirst :: (a -> b) -> (a, x) -> (b, x)
myMapFirst f (a, b) = (f a, b)

parseAny :: Parser Char
parseAny = Parser Data.List.uncons

parseChar :: Char -> Parser Char
parseChar c = parseAny >>= (\a -> if a == c then pure a else empty)

parseAnyChar :: String -> Parser Char
parseAnyChar s = parseAny >>= (\a -> if elem a s then pure a else empty)

parseManyUntil :: Parser a -> Parser b -> Parser [a]
parseManyUntil p1 p2 = (p2 >> (pure []))
    <|> fmap (:) p1 <*> (parseManyUntil p1 p2)

parseString :: String -> Parser String
parseString [] = pure []
parseString (x:xs) = (:) <$> parseChar x <*> parseString xs

parseWhitespace :: Parser ()
parseWhitespace = void $ parseAnyChar " \t\n\r\f\v"

parseEof :: Parser ()
parseEof = Parser (\s -> if length s == 0 then pure ((),s) else empty)

parseSatisfy :: (Char -> Bool) -> Parser Char
parseSatisfy predicate = parseAny
    >>= (\c -> if predicate c then pure c else empty)

parseSepBy :: Parser a -> Parser b -> Parser [a]
parseSepBy p1 p2 = (:) <$> p1 <*> (many (p2 *> p1))

parseOpener :: Char -> Parser ()
parseOpener c = void $ parseChar c *> many parseWhitespace

parseCloser :: Char -> Parser ()
parseCloser c = void $ many parseWhitespace *> parseChar c

_parseExpo :: Bool -> Parser Double
_parseExpo neg = num<$>(collect <$> ((((Data.List.singleton <$> parseChar '0')
    <* (parseWhitespace <|> parseEof))) <|> alt))
    where
        num n = if neg then (-n) else n
        alt = (:) <$> (parseSatisfy digit) <*> many (parseSatisfy isDigit)
        digit :: Char -> Bool
        digit c = isDigit c && c /= '0'
        collect :: String -> Double
        collect = Data.List.foldl' step 0
        step a c = a * 10 + fromIntegral (digitToInt c)

parseExpo :: Parser Double
parseExpo =
        (parseChar '-' *> _parseExpo True)
    <|> (parseChar '+' *> _parseExpo False)
    <|> _parseExpo False

spaces :: Parser ()
spaces = many parseWhitespace *> pure ()

_parseNumber :: Bool -> Parser Double
_parseNumber neg = num <$> (parseZero <|> alt)
    where
        num n = if neg then (-n) else n
        parseZero =collect<$>(singletonZero <* (parseWhitespace <|> parseEof))
        singletonZero = Data.List.singleton <$> parseChar '0'
        alt = (**) <$> parseBase <*> parseExpo'
        parseBase = (+) <$> parseAlt' <*> parseFraction
        parseAlt' = collect <$> (alt' <|> parseString "0")
        alt' = (:) <$> (parseSatisfy digit) <*> many (parseSatisfy isDigit)
        parseFraction = ((parseChar '.') *> (collectF <$> flt)) <|> pure 0.0
        flt = some (parseSatisfy isDigit)
        parseExpo' = (parseAnyChar "eE" *> parseExpo) <|> (pure 1.0)
        digit c = isDigit c && c /= '0'
        collectF s = (collect s) / (10 ^ (length s))
        collect = Data.List.foldl' step 0
        step a c = a * 10 + fromIntegral (digitToInt c)

parseNumber :: Parser Double
parseNumber = (parseChar '-' *> _parseNumber True) <|> _parseNumber False
