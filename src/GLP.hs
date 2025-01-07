module GLP where

import MyParser
import Structures

import Control.Applicative
import Data.Functor

data ParserConfig = ParserConfig { parseBoolean :: Parser Expr, parserOperator :: Parser Expr }

unquote :: String -> String
unquote [] = ""
unquote str = init $ tail str

createBooleanParser :: [String] -> Parser Expr
createBooleanParser (x:y:[]) = fmap Integer (parseGivenString x $> 1 <|> parseGivenString y $> 0)
createBooleanParser _ = fail "Invalid boolean configuration: expected two values."

parseBooleanConfig :: Parser (Parser Expr)
parseBooleanConfig = do
    parseGivenString "boolean:" *> parseWhiteSpaces *> parseChar '[' *> parseWhiteSpaces
    boolValues <- parseSepBy parseStringInQuotes (parseGivenString "," *> parseWhiteSpaces)
    parseChar ']' *> parseWhiteSpaces
    pure $ createBooleanParser boolValues

createOperatorParser :: [String] -> Parser Expr
createOperatorParser (op:"expression":"expression":[]) = ArithmeticOp <$> parseGivenString (unquote op) <*> parseExpression <*> parseExpression
createOperatorParser ("expression":op:"expression":[]) = do
    leftExpr <- parseExpression
    ArithmeticOp <$> parseGivenString (unquote op) <*> leftExpr <*> parseExpression
createOperatorParser ("expression":"expression":op:[]) = do
    leftExpr <- parseExpression
    rightExpr <- parseExpression
    ArithmeticOp <$> parseGivenString (unquote op) <*> leftExpr <*> rightExpr
createOperatorParser _ = fail "Invalid Operator configuration: expected 3 elements defined (operator, right expression, left expression)."

parsePlusConfig :: Parser (Parser Expr)
parsePlusConfig = do
    parseGivenString "plus:" *> parseWhiteSpaces
    plusValue <- parseSepBy parseString (parseWhiteSpaces *> parseGivenString "->" *> parseWhiteSpaces)
    pure $ createOperatorParser plusValue

getConfig :: String -> ParserConfig
getConfig config = ParserConfig {
    parseBoolean = case parse parseBooleanConfig config of
        Left _ -> fail "Failed to parse boolean configuration."
        Right parser -> parser,
    parserOperator = case parse parsePlusConfig config of
        Left _ -> fail "Failed to parse operator configuration."
        Right parser -> parser
}

parseConfigTest :: IO ()
parseConfigTest = case parse "plus: \"+\" -> expression -> expression" parsePlusConfig of
    Left err -> putStrLn $ show err
    Right pa -> case parse "TRUE" pa of
        Left err -> putStrLn $ show err
        Right result -> putStrLn $ show result

