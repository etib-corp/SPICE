module GLP where

import MyParser
import Structures
import LispParser

import Control.Applicative
import Data.Functor

data ParserConfig = ParserConfig { parseBoolean' :: Parser Expr, parserOperator :: [Parser Expr] }

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
createOperatorParser (op:"expression":"expression":[]) =
    ArithmeticOp <$>
    parseGivenString (unquote op)
    <*> (parseWhiteSpaces *> parseExpression)
    <*> (parseWhiteSpaces *> parseExpression)
createOperatorParser ("expression":op:"expression":[]) = do
    leftExpr <- parseExpression
    name <- parseWhiteSpaces *> parseGivenString (unquote op)
    rightExpr <- parseWhiteSpaces *> parseExpression
    return $ ArithmeticOp name leftExpr rightExpr
createOperatorParser ("expression":"expression":op:[]) = do
    leftExpr <- parseExpression
    rightExpr <- parseWhiteSpaces *> parseExpression
    name <- parseWhiteSpaces *> parseGivenString (unquote op)
    return $ ArithmeticOp name leftExpr rightExpr
createOperatorParser _ = fail "Invalid Operator configuration: expected 3 elements defined (operator, right expression, left expression)."

parseOperatorConfig' :: Parser (Parser Expr)
parseOperatorConfig' = do
    (parseDeclarator) *> parseWhiteSpaces
    plusValue <- parseSepBy (parseString <|> parseString') (parseWhiteSpaces *> parseGivenString "->" <* parseWhiteSpaces)
    pure $ createOperatorParser plusValue
        where
            parseDeclarator = parseGivenString "plus:" <|> parseGivenString "minus:" <|> parseGivenString "multiply:" <|> parseGivenString "divide:" <|> parseGivenString "modulo:" <|> parseGivenString "equal:"

parseOperatorConfig :: Parser [Parser Expr]
parseOperatorConfig = parseGivenString "operations:" *> parseWhiteSpaces *> parseGivenString "[" *> parseWhiteSpaces *> parseSepBy (parseOperatorConfig') (parseWhiteSpaces *> parseGivenString "," *> parseWhiteSpaces) <* parseGivenString "]"

getConfig :: String -> ParserConfig
getConfig config = ParserConfig {
    parseBoolean' = case parse config parseBooleanConfig of
        Left _ -> fail "Failed to parse boolean configuration."
        Right parser -> parser,
    parserOperator = case parse config parseOperatorConfig of
        Left _ -> fail "Failed to parse operator configuration."
        Right parser -> parser
}

extractValidParser :: String -> [Parser a] -> Either Error a
extractValidParser str [x] = case parse str x of
    Left (Error msg pos) -> Left (Error ("Unknown operator. " ++ msg) pos)
    Right value -> Right value
extractValidParser str (x:xs) = case parse str x of
    Left _ -> extractValidParser str xs
    Right value -> Right value

parseConfigTest :: IO ()
parseConfigTest = case parse "operations: [ minus: expression -> \"-\" -> expression , plus: expression -> \"+\" -> expression ]" parseOperatorConfig of
    Left err -> putStrLn $ show err
    Right pa -> case extractValidParser "1 + 2" pa of
        Left err -> putStrLn $ show err
        Right result -> putStrLn $ show result
