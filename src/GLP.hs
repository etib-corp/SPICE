module GLP where

import MyParser
import Structures

import Control.Applicative
import Data.Functor

createBooleanParser :: [String] -> Parser Expr
createBooleanParser (x:y:[]) = fmap Integer (parseGivenString x $> 1 <|> parseGivenString y $> 0)
createBooleanParser _ = fail "Invalid boolean configuration: expected two values."

parseBooleanConfig :: Parser (Parser Expr)
parseBooleanConfig = do
    parseGivenString "boolean:" *> parseWhiteSpaces *> parseChar '[' *> parseWhiteSpaces
    boolValues <- parseSepBy parseStringInQuotes (parseGivenString "," *> parseWhiteSpaces)
    parseChar ']' *> parseWhiteSpaces
    pure $ createBooleanParser boolValues

parseConfigTest :: IO ()
parseConfigTest = case parse "boolean: [\"TRUE\", \"faux\"]" parseBooleanConfig of
    Left err -> putStrLn $ show err
    Right pa -> case parse "TRUE" pa of
        Left err -> putStrLn $ show err
        Right result -> putStrLn $ show result


