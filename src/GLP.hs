module GLP where

import MyParser
import Structures
import LispParser

import Control.Applicative
import Data.Functor
import Data.Char

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

loopedParser :: [String] -> Parser String
loopedParser [] = fail "Can not parse any elements of given table."
loopedParser (x:xs) = parseGivenString x <|> loopedParser xs

createVariableConfig' :: [String] -> Parser Expr
createVariableConfig' (('[':ys):"name":[]) = case parse ys (parseSepBy parseStringInQuotes (parseGivenString "," *> parseWhiteSpaces) <* parseGivenString "]" <* parseWhiteSpaces) of
    Left err -> fail "Invalid `variable` configuration, certainly there is a missing ']'"
    Right result -> fmap Var (loopedParser result *> parseWhiteSpaces *> (parseName <|> fail "Invalid variable name."))
createVariableConfig' ("name":('[':ys):[]) = case parse ys (parseSepBy parseStringInQuotes (parseGivenString "," *> parseWhiteSpaces) <* parseGivenString "]" <* parseWhiteSpaces) of
    Left err -> fail "Invalid `variable` configuration, certainly there is a missing ']'"
    Right result -> fmap Var ((parseName <* parseWhiteSpaces <* loopedParser result) <|> fail "Invalid variable name.")
createVariableConfig' tab = case length tab of
    1 -> fail ("Invalid `variable` configuration, not enough elements which are defining a variable syntax. " ++ show tab)
    _ -> fail ("Invalid `variable` configuration, too many elements which are defining a variable syntax. " ++ show tab)

createVariableConfig :: [String] -> Formatter -> Parser Expr
createVariableConfig [] _ = fail "Invalid `variable` configuration."
createVariableConfig content (p,s) = parseGivenString p *> createVariableConfig' content <* parseGivenString s

parseVariableConfig :: Parser (Parser Expr)
parseVariableConfig = do
    formatters <- parseGivenString "variable" *> parseFormatters <* parseGivenString ":" <* parseWhiteSpaces
    first <- parseManyUntil parseAnyChar (parseChar ' ') <* parseWhiteSpaces <* parseGivenString "->" <* parseWhiteSpaces
    second <- (parseConfigString <|> many (satisfy (isPrint)))
    pure $ createVariableConfig [first,second] formatters

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

parseTestCodeBlock :: IO ()
parseTestCodeBlock = case parse "codeBlock{prefix: \"{\", suffix: \"}\"} : [\".\", \"\n\", \";\"] -> many(expression)" parseCodeBlockConfig of
    Left err -> putStrLn $ show err
    Right pa -> case parse "{#t\n1}" pa of
        Left err -> putStrLn $ show err
        Right result -> putStrLn $ show result

parseTable' :: String -> Either Error [String]
parseTable' tab = parse tab (parseSepBy parseStringInQuotes (parseGivenString "," *> parseWhiteSpaces) <* parseGivenString "]" <* parseWhiteSpaces)

createFunctionParser' :: [String] -> [String] -> Formatter -> Parser [String] -> Parser Expr -> Parser Expr
createFunctionParser' ("declarator":"name":"parameters":"codeBlock":[]) declarators (p,s) par cod = do
    declarator <- parseGivenString p *> loopedParser declarators <* parseWhiteSpaces
    name <- parseName <* parseWhiteSpaces
    parameters <- par <* parseWhiteSpaces
    codeBlock <- cod <* parseWhiteSpaces
    pure $ Function name parameters codeBlock
createFunctionParser' ("declarator":"parameters":"name":"codeBlock":[]) declarators (p,s) par cod = do
    declarator <- parseGivenString p *> loopedParser declarators <* parseWhiteSpaces
    parameters <- par <* parseWhiteSpaces
    name <- parseName <* parseWhiteSpaces
    codeBlock <- cod <* parseWhiteSpaces
    pure $ Function name parameters codeBlock
createFunctionParser' ("name":"declarator":"parameters":"codeBlock":[]) declarators (p,s) par cod = do
    name <- parseName <* parseWhiteSpaces
    declarator <- parseGivenString p *> loopedParser declarators <* parseWhiteSpaces
    parameters <- par <* parseWhiteSpaces
    codeBlock <- cod <* parseWhiteSpaces
    pure $ Function name parameters codeBlock
createFunctionParser' ("name":"parameters":"declarator":"codeBlock":[]) declarators (p,s) par cod = do
    name <- parseName <* parseWhiteSpaces
    parameters <- par <* parseWhiteSpaces
    declarator <- parseGivenString p *> loopedParser declarators <* parseWhiteSpaces
    codeBlock <- cod <* parseWhiteSpaces
    pure $ Function name parameters codeBlock
createFunctionParser' _ _ _ _ _ = fail "Invalid `function` configuration."

createFunctionParser :: [String] -> Formatter -> Parser [String] -> Parser Expr -> Parser Expr
createFunctionParser (('[':xs):y:z:a:[]) f p c = case parseTable' xs of
    Left err -> fail ("Invalid declarator definition in `function` configuration. ")
    Right value -> createFunctionParser' ["declarator",y,z,a] value f p c
createFunctionParser (y:('[':xs):z:a:[]) f p c = case parseTable' xs of
    Left err -> fail ("Invalid declarator definition in `function` configuration. ")
    Right value -> createFunctionParser' [y,"declarator",z,a] value f p c
createFunctionParser (y:z:('[':xs):a:[]) f p c= case parseTable' xs of
    Left err -> fail ("Invalid declarator definition in `function` configuration. ")
    Right value -> createFunctionParser' [y,z,"declarator",a] value f p c
createFunctionParser (y:z:a:('[':xs):[]) f p c = case parseTable' xs of
    Left err -> fail ("Invalid declarator definition in `function` configuration. ")
    Right value -> createFunctionParser' [y,z,a,"declarator"] value f p c
createFunctionParser tab _ _ _ = case length tab > 4 of
    True -> fail "Invalid `function` configuration: too many elements which are defining a function syntax."
    False -> fail "Invalid `function` configuration: not enough elements which are defining a function syntax."

parseFunctionConfig :: Parser [String] -> Parser Expr -> Parser (Parser Expr)
parseFunctionConfig p c = do
    formatters <- parseGivenString "function" *> parseFormatters <* parseWhiteSpaces
    content <- parseSepBy (parseConfigString) (parseGivenString "->" *> parseWhiteSpaces)
    pure $ createFunctionParser content formatters p c

createIfParser :: [String] -> Formatter -> Parser Expr -> Parser Expr -> Parser Expr
createIfParser (('[':xs):('[':ys):[]) (p,s) cond cod = case parseTable' xs of
    Left errP -> fail "Invalid code prefix for `if` configuration."
    Right pref -> case parseTable' ys of
        Left errS -> fail "Invalid code suffix for `if` configuration."
        Right suf -> do
            parseGivenString p *> parseWhiteSpaces *> loopedParser pref
            condition <- cond <* parseWhiteSpaces
            fstBlock <- cod <* parseWhiteSpaces
            parseWhiteSpaces *> loopedParser suf *> parseWhiteSpaces
            sndBlock <- cod <* parseWhiteSpaces <* parseGivenString s
            pure $ If condition fstBlock sndBlock
createIfParser tab _ _ _ = case length tab > 2 of
    True -> fail "Invalid `if` configuration: too many elements which are defining a if syntax."
    False -> fail "Invalid `if` configuration: not enough elements which are defining a if syntax."

parseIfconfig :: Parser Expr -> Parser Expr -> Parser (Parser Expr)
parseIfconfig cond cod = do
    formatters <- parseGivenString "if" *> parseFormatters <* parseWhiteSpaces <* parseGivenString ":"
    content <- parseSepBy (parseConfigString) (parseGivenString "->" *> parseWhiteSpaces)
    pure $ createIfParser content formatters cond cod

parseConfigTest :: IO ()
parseConfigTest = case parse "variable{prefix: \"(\", suffix: \")\"}: name -> [\"define\"]" parseVariableConfig of
    Left err -> putStrLn $ show err
    Right pa -> case parse "(a define)" pa of
        Left err -> putStrLn $ show err
        Right result -> putStrLn $ show result
