{-# LANGUAGE RankNTypes #-}

module MyParser where

import Lib

import Control.Applicative
import Control.Monad
import Data.Functor.Identity
import Data.List
import Data.Char
import Debug.Trace

data Error = Error { msg :: String, pos :: Int } deriving (Show)
data State = State { str :: String, position :: Int} deriving (Show)
data ParsecT m a = ParsecT {
        runParser ::
            forall b .
            State ->
            (a -> State -> m b) ->
            (Error -> State -> m b) ->
            m b
    }

instance Functor (ParsecT m) where
    fmap f p = ParsecT (\ s ok ko -> runParser p s (ok . f) ko)

instance Applicative (ParsecT m) where
    pure e = ParsecT (\ s ok _ -> ok e s)
    f <*> p = ParsecT $ \ s ok ko ->
        let ok' f' s' = runParser p s' (ok . f') ko
        in runParser f s ok' ko

instance Alternative (ParsecT m) where
    empty = ParsecT (\ s _ ko -> ko (Error "empty" (position s)) s)
    pa <|> pb = ParsecT $ \ s ok ko ->
        let error' _ _ = runParser pb s ok ko
        in runParser pa s ok error'

instance Monad (ParsecT m) where
    pa >>= func = ParsecT $ \ s ok ko ->
        let ok' r s' = runParser (func r) s' ok ko
        in runParser pa s ok' ko

instance MonadFail (ParsecT m) where
    fail message = ParsecT (\ s _ ko -> ko (Error message (position s)) s)

type Parser = ParsecT Identity

parseT :: (Applicative m) => State -> ParsecT m a -> m (Either Error a)
parseT s pt = runParser pt s ok ko
    where
        ok x _ = pure $ Right x
        ko err _ = pure $ Left err

parse :: String -> Parser a -> Either Error a
parse s pa = runIdentity $ parseT (State s 0) pa

parseAnyChar :: Parser Char
parseAnyChar = ParsecT $ \ s ok ko -> case uncons $ str s of
    Nothing -> ko (Error "End Of File" (position s)) s
    Just (h,t) -> ok h $ State t ((position s) + 1)

satisfy :: (Char -> Bool) -> Parser Char
satisfy func = parseAnyChar >>=
    (\ c -> if func c then pure c else fail "Char doesn't exist... but how ?")

parseChar :: Char -> Parser Char
parseChar c = satisfy (== c)

parseOneOf :: String -> Parser Char
parseOneOf s = satisfy (\ c -> elem c s)

parseNoneOf :: String -> Parser Char
parseNoneOf s = satisfy (\ c -> not $ elem c s)

parseString :: Parser String
parseString = parseManyUntil parseAnyChar (satisfy isWhiteSpace)

parseManyUntil :: Parser a -> Parser b -> Parser [a]
parseManyUntil pa pb = (pb *> pure []) <|> fmap (:) pa <*> parseManyUntil pa pb

parseSomeUntil :: Parser a -> Parser b -> Parser [a]
parseSomeUntil pa pb = fmap (:) pa <*> parseManyUntil pa pb

parseWhiteSpaces :: Parser ()
parseWhiteSpaces = void $ many $ satisfy (isSpace)

parseInt :: Parser Int
parseInt = check >>= \ s ->
    case getInt s of
        Nothing -> fail "Invalid signed number"
        Just x -> pure x
    where
        check = (fmap (:) (parseChar '-') <*> some (satisfy isDigit)) <|> some (satisfy isDigit)

parseUInt :: Parser Int
parseUInt = some (satisfy isDigit) >>= \ s ->
    case getInt s of
        Nothing -> fail "Invalid unsigned number"
        Just x -> pure x

parseGivenString :: String -> Parser String
parseGivenString "" = pure ""
parseGivenString s = fmap (:) (parseChar (head s)) <*> parseGivenString (tail s)
