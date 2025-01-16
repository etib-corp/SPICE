module GLP where

import MyParser
import Structures
import LispParser
import Files

import Control.Applicative
import Data.Functor
import Data.Char

import Debug.Trace

type Formatter = (String, String)

data ParserConfig = ParserConfig {
    parseBoolean' :: Parser Expr
  , parseVariable :: Parser Expr
  , parserOperator :: [Parser Expr]
  , parseIf :: Parser Expr
--   , parseFunction :: Parser Expr
  } | NullConfig

unquote :: String -> String
unquote [] = ""
unquote str = init $ tail str

parseConfigString :: Parser String
parseConfigString = parseString <|> parseString'

parsePrefixConfig :: Parser String
parsePrefixConfig = (parseGivenString "prefix:" *> parseWhiteSpaces *> parseStringInQuotes) <|> parseGivenString ""

parseSuffixConfig :: Parser String
parseSuffixConfig = (parseGivenString "suffix:" *> parseWhiteSpaces *> parseStringInQuotes) <|> parseGivenString ""

useOps :: [Parser Expr] -> Parser Expr
useOps (x:xs) = x <|> useOps xs
useOps [] = fail "empty list..."

parseExpressionConfig :: ParserConfig -> Parser Expr
parseExpressionConfig (ParserConfig pbool pvar pops pif) = parseInteger <|> pbool <|> pvar <|> (useOps pops) <|> pif
-- parseExpressionConfig _ = fail "failed to parse expression"

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
    (parseGivenString p *> parseGivenString op)
    <*> (parseWhiteSpaces *> parseExpression)
    <*> (parseWhiteSpaces *> parseExpression)
    <* parseGivenString s
createOperatorParser ("expression":op:"expression":[]) (p,s) = do
    leftExpr <- parseGivenString p *> parseExpression
    name <- parseWhiteSpaces *> parseGivenString op
    rightExpr <- parseWhiteSpaces *> parseExpression <* parseGivenString s
    return $ ArithmeticOp name leftExpr rightExpr
createOperatorParser ("expression":"expression":op:[]) (p,s) = do
    leftExpr <- parseGivenString p *> parseExpression
    rightExpr <- parseWhiteSpaces *> parseExpression
    name <- parseWhiteSpaces *> parseGivenString op <* parseGivenString s
    return $ ArithmeticOp name leftExpr rightExpr
createOperatorParser t _ =  if (length t) >= 3 then fail ">3" else fail "<3"

parseOperatorConfig' :: Formatter -> Parser (Parser Expr)
parseOperatorConfig' f = do
    parseWhiteSpaces *> parseDeclarator <* parseWhiteSpaces
    operatorValue <- parseSepBy (parseString' <|> parseStringInQuotes) (parseWhiteSpaces *> parseGivenString "->" <* parseWhiteSpaces)
    pure $ createOperatorParser operatorValue f
        where
            parseDeclarator = parseGivenString "plus:" <|> parseGivenString "minus:" <|> parseGivenString "multiply:" <|> parseGivenString "divide:" <|> parseGivenString "modulo:" <|> parseGivenString "equal:" <|> parseGivenString "assignation:"

parseOperatorConfig :: Parser [Parser Expr]
parseOperatorConfig = do
    parseGivenString "operators"
    formatters <- parseFormatters
    parseGivenString ":" *> parseWhiteSpaces *> parseGivenString "[" *> parseWhiteSpaces *> parseSepBy (parseOperatorConfig' formatters) (parseGivenString ",") <* parseGivenString "]"

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

createCodeBlockParser :: Formatter -> [String] -> Parser Expr
createCodeBlockParser (p,s) (sep:l) = List <$> ((parseGivenString p) *>
    parseSepBy parseExpression (parseGivenString sep)
    <* parseGivenString s) <|> createCodeBlockParser (p,s) l -- parseExpressionConfig eventually
createCodeBlockParser _ _ = fail "Invalid code block"

parseCodeBlockConfig :: Parser (Parser Expr)
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
    Right pa -> case parse "{#t;1}" pa of
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
    formatters <- parseGivenString "function" *> parseFormatters <* parseWhiteSpaces <* parseGivenString ":" <* parseWhiteSpaces
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

parseIfConfig :: Parser Expr -> Parser Expr -> Parser (Parser Expr)
parseIfConfig cond cod = do
    formatters <- parseGivenString "if" *> parseFormatters <* parseWhiteSpaces <* parseGivenString ":" <* parseWhiteSpaces
    content <- parseSepBy (parseConfigString) (parseGivenString "->" *> parseWhiteSpaces)
    pure $ createIfParser content formatters cond cod

parseConfigTest :: IO ()
parseConfigTest = case parse "variable{prefix: \"(\", suffix: \")\"}: name -> [\"define\"]" parseVariableConfig of
    Left err -> putStrLn $ show err
    Right pa -> case parse "(a define)" pa of
        Left err -> putStrLn $ show err
        Right result -> putStrLn $ show result

getBooleanParserTest :: Parser Expr
getBooleanParserTest = case parse "boolean{prefix: \"(\", suffix: \")\"}: [\"#t\", \"#f\"]" parseBooleanConfig of
    Left err -> fail "Error boolean"
    Right p -> p

getOperatorsParserTest :: [Parser Expr]
getOperatorsParserTest = case parse "operators{prefix: \"(\", suffix: \")\"}: [plus: \"+\" -> expression -> expression, minus:\"-\" -> expression -> expression, multiply: \"*\" -> expression -> expression, divide: \"/\" -> expression -> expression]" parseOperatorConfig of
    Left err -> fail "error with operators"
    Right p -> p

getVariableParserTest :: Parser Expr
getVariableParserTest = case parse "variable{prefix: \"(\", suffix: \")\"}: [\"define\"] -> name" parseVariableConfig of
    Left _ -> fail "error with variable"
    Right p -> p

getConditionParserTest :: Parser Expr
getConditionParserTest = case parse "condition{prefix: \"(\", suffix: \")\"}: expression" parseConditionConfig of
    Left _ -> fail "error with condition"
    Right p -> p

getParameterParserTest :: Parser Expr
getParameterParserTest = case parse "parameters{prefix: \"(\", suffix: \")\"}: name -> \" \"" parseConditionConfig of
    Left _ -> fail "error with parameters"
    Right p -> p

getCodeBlockParserTest :: Parser Expr
getCodeBlockParserTest = case parse "codeBlock{prefix: \"(\", suffix: \")\"}: many(expression)" parseCodeBlockConfig of
    Left _ -> fail "error with code block"
    Right p -> p

getIfParserTest :: Parser Expr -> Parser Expr -> Parser Expr
getIfParserTest pa pb = case parse "if{prefix: \"(\", suffix: \")\"}: [\"if\"] -> [\"\"]" (parseIfConfig pa pb) of
    Left _ -> fail "error with if"
    Right p -> p

getFunctionParserTest :: Parser [String] -> Parser Expr -> Parser Expr
getFunctionParserTest ps pa = case parse "function{prefix: \"(\", suffix: \")\"}: [\"define\"] -> name -> parameters -> codeBlock" (parseFunctionConfig ps pa) of
    Left _ -> fail "error with function"
    Right p -> p

testConfig ::String -> IO()
testConfig s = do
    let codeblock = getCodeBlockParserTest
    let parameters = getParameterParserTest
    let config = (ParserConfig getBooleanParserTest getVariableParserTest getOperatorsParserTest (getIfParserTest parameters codeblock))
    case parse s (parseExpressionConfig config) of
        Left err -> putStrLn $ show err
        Right result -> print result

isInGoodExtension :: String -> Bool
isInGoodExtension "" = False
isInGoodExtension str = case length str == 6 of
    True -> str == ".spice"
    False -> isInGoodExtension $ tail str

parseSyntaxConfiguration :: [String] -> Maybe ParserConfig
parseSyntaxConfiguration tab | length tab == 8 = case parse (tab !! 0) parseBooleanConfig of
    Right bool -> case parse (tab !! 1) parseVariableConfig of
        Right var -> case parse (tab !! 2) parseOperatorConfig of
            Right op -> case parse (tab !! 3) parseConditionConfig of
                Right cond -> case parse (tab !! 4) parseParametersConfig of
                    Right param -> case parse (tab !! 5) parseCodeBlockConfig of
                        Right code -> case parse (tab !! 6) (parseIfConfig cond code) of
                            Right ifConf -> case parse (tab !! 7) (parseFunctionConfig param code) of
                                Right func -> Just $ ParserConfig bool var op ifConf
                                Left _ -> trace ("fail function") Nothing
                            Left _ -> trace ("fail if") Nothing
                        Left _ -> trace ("fail code") Nothing
                    Left _ -> trace ("fail param") Nothing
                Left _ -> trace ("fail cond") Nothing
            Left _ -> trace ("fail op") Nothing
        Left _ -> trace ("fail op") Nothing
    Left _ -> trace ("fail var") Nothing
                            | otherwise = Nothing

getParserConfiguration :: String -> Maybe ParserConfig
getParserConfiguration "" = Nothing
getParserConfiguration str = parseSyntaxConfiguration $ lines str

testParserConfiguration :: String -> IO ParserConfig
testParserConfiguration content = case getParserConfiguration content of
    Just config -> pure config
    Nothing -> pure NullConfig

loadParserConfiguration :: String -> IO ParserConfig
loadParserConfiguration path = secureGetContent path >>= testParserConfiguration
