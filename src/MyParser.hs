{-# LANGUAGE RankNTypes #-}

-- | This module provides a custom parser combinator library. It includes
-- utilities to define, combine and run parsers with detailed error handling.
module MyParser where

import Lib

import Control.Applicative
import Control.Monad

import Data.Functor.Identity
import Data.List
import Data.Char

-- | Represents an error in the parsing process, including a message and
-- the position in the input where the error occurred.
data Error = Error { msg :: String, pos :: Int } deriving (Show)

-- Ajouter l'instance Eq pour pouvoir comparer les erreurs dans les tests
instance Eq Error where
    (Error msg1 pos1) == (Error msg2 pos2) = msg1 == msg2 && pos1 == pos2

-- | Represents the state of the parser, including the remaining input
-- string and the current position.
data State = State { str :: String, position :: Int} deriving (Show)

-- | A parser transformer type that encapsulates a parsing computation
-- with support for error handling and state management.
data ParsecT m a = ParsecT {
        runParser ::
            forall b .
            State ->
            (a -> State -> m b) ->
            (Error -> State -> m b) ->
            m b
    }

-- Functor instance for 'ParsecT', allowing transformation of parsed values.
instance Functor (ParsecT m) where
    fmap f p = ParsecT (\ s ok ko -> runParser p s (ok . f) ko)

-- Applicative instance for 'ParsecT', supporting sequential application.
instance Applicative (ParsecT m) where
    pure e = ParsecT (\ s ok _ -> ok e s)
    f <*> p = ParsecT $ \ s ok ko ->
        let ok' f' s' = runParser p s' (ok . f') ko
        in runParser f s ok' ko

-- Alternative instance for 'ParsecT', enabling choice between parsers.
instance Alternative (ParsecT m) where
    empty = ParsecT (\ s _ ko -> ko (Error "empty" (position s)) s)
    pa <|> pb = ParsecT $ \ s ok ko ->
        let error' _ _ = runParser pb s ok ko
        in runParser pa s ok error'

-- Monad instance for 'ParsecT', enabling sequencing of parsing operations.
instance Monad (ParsecT m) where
    pa >>= func = ParsecT $ \ s ok ko ->
        let ok' r s' = runParser (func r) s' ok ko
        in runParser pa s ok' ko

-- MonadFail instance for 'ParsecT', supporting failure with a custom message.
instance MonadFail (ParsecT m) where
    fail message = ParsecT
        (\ s _ ko -> ko (Error (message ++ " !! Error occured here:-> '" ++ (getUntilBackspace (str s)) ++ "'. !!") (position s)) s)

-- | A specialized type alias for parsers that do not involve additional monads.
type Parser = ParsecT Identity

-- | Runs a parser on the given state and returns the result.
parseT :: (Applicative m) => State -> ParsecT m a -> m (Either Error a)
parseT s pt = runParser pt s ok ko
    where
        ok x _ = pure $ Right x
        ko err _ = pure $ Left err

-- | A helper function to run a parser on a string input.
parse :: String -> Parser a -> Either Error a
parse s pa = runIdentity $ parseT (State s 0) pa

-- | Parses a valid name, starting with a letter and followed by alphanumeric characters.
parseName :: Parser String
parseName = do
    c <- parseOneOf "abcdefghijklmnopqrstuvwxyzQWERTYUIOPASDFGHJKLZXCVBNM"
    cs <- many $ parseOneOf "abcdefghijklmnopqrstuvwxyz0123456789QWERTYUIOPASDFGHJKLZXCVBNM"
    pure (c:cs)

-- | Parses a char, returns an error if it encounters EOF.
parseAnyChar :: Parser Char
parseAnyChar = ParsecT $ \ s ok ko -> case uncons $ str s of
    Nothing -> ko (Error "End Of File" (position s)) s
    Just (h,t) -> ok h $ State t ((position s) + 1)


-- | Parses a char only if it respects the condition passed as argument.
satisfy :: (Char -> Bool) -> Parser Char
satisfy func = parseAnyChar >>=
    (\ c -> if func c then pure c else fail ("Failed to satisfy needed char. Got: '" ++ [c] ++ "'."))

-- | Parses a char only if it corresponds to the char given as argument.
parseChar :: Char -> Parser Char
parseChar c = satisfy (== c)

-- | Parses a char only if it takes part of the string given as argument.
parseOneOf :: String -> Parser Char
parseOneOf s = satisfy (\ c -> elem c s)

-- | Parses a char only if it does not takes part of the string given as argument.
parseNoneOf :: String -> Parser Char
parseNoneOf s = satisfy (\ c -> not $ elem c s)

-- | Parses a string, the end of the string is delimited by whitespaces:
-- ('\t, '\n', '\r', ' ').
parseString :: Parser String
parseString = parseSomeUntil parseAnyChar (satisfy isWhiteSpace)

parseString' :: Parser String
parseString' = some (satisfy isAlpha)

-- | Parses while it can parse with a separator parser given as parameter.
parseSepBy :: Parser a -> Parser b -> Parser [a]
parseSepBy p1 p2 = (:) <$> p1 <*> (many (p2 *> p1))

parseSepBy' :: Parser a -> Parser b -> Parser [a]
parseSepBy' p1 p2 = (:) <$> p1 <*> (some (p2 *> p1))

-- | Parses with a first parser while it parses using the second parser given as argument.
parseManyUntil :: Parser a -> Parser b -> Parser [a]
parseManyUntil pa pb = (pb *> pure []) <|> fmap (:) pa <*> parseManyUntil pa pb

-- | Parses with a first parser while it parses using the second parser given as argument.
parseSomeUntil :: Parser a -> Parser b -> Parser [a]
parseSomeUntil pa pb = fmap (:) pa <*> parseManyUntil pa pb

-- | Parses while it corresponds to a whitespace from a string.
parseWhiteSpaces :: Parser ()
parseWhiteSpaces = void $ many $ satisfy (isSpace)

-- | Parses only integer value from a string.
parseInt :: Parser Int
parseInt = check >>= \ s ->
    case getInt s of
        Nothing -> fail ("Invalid signed number, here is your number: '" ++ s ++ "'.")
        Just x -> pure x
    where
        check = ((fmap (:) (parseChar '-') <*> some (satisfy isDigit)) <|> some (satisfy isDigit)) <|> fail ("Invalid signed number.")

-- | Parses only unsigned integers from a string.
parseUInt :: Parser Int
parseUInt = (some (satisfy isDigit) <|> fail "Invalid unsigned number") >>= \ s ->
    case getInt s of
        Nothing -> fail "Invalid unsigned number"
        Just x -> pure x

-- | Parses only if it corresponds to the string given as parameter.
parseGivenString :: String -> Parser String
parseGivenString "" = pure ""
parseGivenString s = (fmap (:) (parseChar (head s)) <*> parseGivenString (tail s)) <|> fail ("String does not match. Expected: '" ++ s ++ "'.")

-- | Parses only double value.
parseDouble :: Parser Double
parseDouble = check >>= \ s ->
    case getDouble s of
        Nothing -> fail "Invalid float"
        Just x -> pure x
    where
        check = (fmap (++) (fmap (++) (fmap (:) (parseChar '-') <*> some (satisfy isDigit)) <*> parseGivenString ".") <*> some (satisfy isDigit)) <|> fail "Invalid float."

parseStringInQuotes :: Parser String
parseStringInQuotes = parseChar '"' *> parseManyUntil parseAnyChar (satisfy (== '"'))
