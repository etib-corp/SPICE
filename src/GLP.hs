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

-- parseTestConfig :: Parser (ConfigExpr)
-- parseTestConfig = do
--     parseGivenString "test"
--     a <- parseFormatters
--     pure $ parseExprInFormatter a

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

-- getConfig :: String -> ParserConfig
-- getConfig config = ParserConfig {
--     parseBoolean' = case parse config parseBooleanConfig of
--         Left _ -> fail "Failed to parse boolean configuration."
--         Right parser -> parser,
--     parserOperator = case parse config parseOperatorConfig of
--         Left _ -> fail "Failed to parse operator configuration."
--         Right parser -> parser
-- }

extractValidParser :: String -> [Parser a] -> Either Error a
extractValidParser str [x] = case parse str x of
    Left (Error msg pos) -> Left (Error ("Unknown operator. " ++ msg) pos)
    Right value -> Right value
extractValidParser str (x:xs) = case parse str x of
    Left _ -> extractValidParser str xs
    Right value -> Right value

-- parseExprInFormatter :: Formatter -> ConfigExpr
-- parseExprInFormatter (left, right) = parseGivenString left *> parseExpression <* parseGivenString right
-- parseExprInFormatter _ = fail "Failed to parse formatters"

-- parseConfigTestFormatter :: String -> IO()
-- parseConfigTestFormatter s = case parse "test{prefix: \"<\"}" parseTestConfig of
--     Left err -> putStrLn $ show err
--     Right p -> case parse s p of
--         Left err -> putStrLn $ show err
--         Right result -> putStrLn $ show result

parseConfigTest :: IO ()
parseConfigTest = case parse "operators{prefix: \"(\", suffix: \")\"}: [plus: \"+\" -> expression -> expression , minus: \"-\" -> expression -> expression , multiply: \"*\" -> expression -> expression , divide: \"/\" -> expression -> expression , modulo: \"%\" -> expression -> expression , equal: \"eq?\" -> expression -> expression ]" parseOperatorConfig of
    Left err -> putStrLn $ show err
    Right pa -> case extractValidParser "(+ 1 2)" pa of
        Left err -> putStrLn $ show err
        Right result -> putStrLn $ show result
