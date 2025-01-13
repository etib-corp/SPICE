module GLP where

import MyParser
import Structures
import LispParser

import Control.Applicative
import Data.Functor

type Formatter = (String, String)

data ParserConfig = ParserConfig { parseBoolean' :: Parser Expr, parserOperator :: [Parser Expr] }

unquote :: String -> String
unquote [] = ""
unquote str = init $ tail str

parseConfigString :: Parser String
parseConfigString = parseString <|> parseString'

parsePrefixConfig :: Parser String
parsePrefixConfig = (parseGivenString "prefix:" *> parseWhiteSpaces *> parseStringInQuotes) <|> parseGivenString ""

parseSuffixConfig :: Parser String
parseSuffixConfig = (parseGivenString "suffix:" *> parseWhiteSpaces *> parseStringInQuotes) <|> parseGivenString ""

parseExpressionConfig :: Parser Expr
parseExpressionConfig = parseInteger <|> parseBoolean

parseFormatters :: Parser Formatter
parseFormatters = do
    parseGivenString "{" *> parseWhiteSpaces
    prefix <- parsePrefixConfig
    ((parseWhiteSpaces *> parseGivenString ",") <|> parseGivenString "") *> parseWhiteSpaces
    suffix <- parseSuffixConfig
    parseGivenString "}"
    pure $ (prefix, suffix)

createBooleanParser :: [String] -> Formatter -> Parser Expr
createBooleanParser (x:y:[]) (p,s) = fmap Integer (parseGivenString p *> (parseGivenString x $> 1 <|> parseGivenString y $> 0) <* parseGivenString s)
createBooleanParser _ _ = fail "Invalid boolean configuration: expected two values."

parseBooleanConfig :: Parser (Parser Expr)
parseBooleanConfig = do
    formatter <- parseGivenString "boolean" *> parseFormatters
    parseGivenString ":" *> parseWhiteSpaces *> parseChar '[' *> parseWhiteSpaces
    boolValues <- parseSepBy parseStringInQuotes (parseGivenString "," *> parseWhiteSpaces)
    parseChar ']' *> parseWhiteSpaces
    pure (createBooleanParser boolValues formatter)

createOperatorParser :: [String] -> Formatter -> Parser Expr
createOperatorParser (op:"expression":"expression":[]) (p,s) =
    ArithmeticOp <$>
    (parseGivenString p *> parseGivenString (unquote op))
    <*> (parseWhiteSpaces *> parseExpression)
    <*> (parseWhiteSpaces *> parseExpression)
    <* parseGivenString s
createOperatorParser ("expression":op:"expression":[]) (p,s) = do
    leftExpr <- parseGivenString p *> parseExpression
    name <- parseWhiteSpaces *> parseGivenString (unquote op)
    rightExpr <- parseWhiteSpaces *> parseExpression <* parseGivenString s
    return $ ArithmeticOp name leftExpr rightExpr
createOperatorParser ("expression":"expression":op:[]) (p,s) = do
    leftExpr <- parseGivenString p *> parseExpression
    rightExpr <- parseWhiteSpaces *> parseExpression
    name <- parseWhiteSpaces *> parseGivenString (unquote op) <* parseGivenString s
    return $ ArithmeticOp name leftExpr rightExpr
createOperatorParser _ _ = fail "Invalid Operator configuration: expected 3 elements defined (operator, right expression, left expression)."

parseOperatorConfig' :: Formatter -> Parser (Parser Expr)
parseOperatorConfig' f = do
    (parseDeclarator) *> parseWhiteSpaces
    operatorValue <- parseSepBy parseConfigString (parseWhiteSpaces *> parseGivenString "->" <* parseWhiteSpaces)
    pure $ createOperatorParser operatorValue f
        where
            parseDeclarator = parseGivenString "plus:" <|> parseGivenString "minus:" <|> parseGivenString "multiply:" <|> parseGivenString "divide:" <|> parseGivenString "modulo:" <|> parseGivenString "equal:" <|> parseGivenString "assignation:"

parseOperatorConfig :: Parser [Parser Expr]
parseOperatorConfig = do
    parseGivenString "operators"
    formatters <- parseFormatters
    parseGivenString ":" *> parseWhiteSpaces *> parseGivenString "[" *> parseWhiteSpaces *> parseSepBy (parseOperatorConfig' formatters) (parseWhiteSpaces *> parseGivenString "," *> parseWhiteSpaces) <* parseGivenString "]"

extractValidParser :: String -> [Parser a] -> Either Error a
extractValidParser str [x] = case parse str x of
    Left (Error msg pos) -> Left (Error ("Unknown operator. " ++ msg) pos)
    Right value -> Right value
extractValidParser str (x:xs) = case parse str x of
    Left _ -> extractValidParser str xs
    Right value -> Right value

createParametersConfig :: [String] -> Formatter -> Parser [String]
createParametersConfig ("name":y:[]) (p,s) = parseGivenString p *> parseWhiteSpaces *> (parseSepBy parseName (parseGivenString y) <|> pure []) <* parseWhiteSpaces <* parseGivenString s
createParametersConfig ("name":[]) (p,s) = parseGivenString p *> parseWhiteSpaces *> (parseSepBy parseName parseWhiteSpaces <|> pure []) <* parseWhiteSpaces <* parseGivenString s
createParametersConfig _ _ = fail "Invalid `parameters` configuration."

parseParametersConfig :: Parser (Parser [String])
parseParametersConfig = do
    formatters <- parseGivenString "parameters" *> parseFormatters
    parseGivenString ":" *> parseWhiteSpaces
    content <- parseSepBy (parseStringInQuotes <|> parseConfigString) (parseGivenString "->" *> parseWhiteSpaces)
    pure $ (createParametersConfig content formatters)

parseConditionConfig :: Parser (Parser Expr)
parseConditionConfig = do
    formatters <- parseGivenString "condition" *> parseFormatters
    (parseGivenString ":" *> parseWhiteSpaces *> parseGivenString "expression") <|> fail "Invalid `condition` configuration."
    pure $ parseExpression

createCodeBlockParser :: Formatter -> [String] -> Parser [Expr]
createCodeBlockParser (p,s) (sep:l) = parseGivenString p *>
    (parseSepBy parseExpressionConfig (parseGivenString sep) <* parseGivenString s) 
    <|> createCodeBlockParser (p,s) l
createCodeBlockParser _ _ = fail "Invalid code block"

parseCodeBlockConfig :: Parser (Parser [Expr])
parseCodeBlockConfig = do
    parseGivenString "codeBlock"
    formatters <- parseFormatters
    parseWhiteSpaces *> parseGivenString ":" *> parseWhiteSpaces
    separators <- parseGivenString "[" *>
        parseSepBy parseStringInQuotes (parseWhiteSpaces *> parseGivenString "," *> parseWhiteSpaces)
        <* parseGivenString "]"
    pure $ createCodeBlockParser formatters separators

parseTestCodeBlock :: IO()
parseTestCodeBlock = case parse "codeBlock{prefix: \"{\", suffix: \"}\"} : [\".\", \"\n\", \";\"] -> many(expression)" parseCodeBlockConfig of
    Left err -> putStrLn $ show err
    Right pa -> case parse "{#t\n1}" pa of
        Left err -> putStrLn $ show err
        Right result -> putStrLn $ show result

parseConfigTest :: IO ()
parseConfigTest = case parse "condition{prefix: \"(\", suffix: \")\"}: expression" parseConditionConfig of
    Left err -> putStrLn $ show err
    Right pa -> case parse "(eq? 1 2)" pa of
        Left err -> putStrLn $ show err
        Right result -> putStrLn $ show result
