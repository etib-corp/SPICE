module GLP where

import MyParser
import Structures
import LispParser
import Files
import Lib
import Formatters
import GLPUtils

import Control.Applicative

import Data.Functor
import Data.Char

import Debug.Trace

data ParserConfig = ParserConfig {
    parseBoolean' :: Parser Expr
  , parseVariable :: Parser Expr
  , parserOperator :: [Parser Expr]
  , parseCondition :: Parser Expr
  , parseParameter :: Parser [String]
  , codeBlockConfig :: (Formatter, [String])
  , parseIf :: (Formatter,[String],[String])
  , parseFunction :: (Formatter, [String], [String])
  } | NullConfig

parseExpressionConfig :: ParserConfig -> Parser Expr
parseExpressionConfig (ParserConfig pbool pvar pops pcond ppar cb ifconf func) =
    (functionParserConfig func (ParserConfig pbool pvar pops pcond ppar cb ifconf func))
    <|> (ifParserConfig ifconf (ParserConfig pbool pvar pops pcond ppar cb ifconf func))
    <|> parseInteger <|> pbool <|> pvar <|> pcond
    <|> (useOps pops)
    <|> (parseCodeBlock cb (ParserConfig pbool pvar pops pcond ppar cb ifconf func))
parseExpressionConfig NullConfig = fail "Invalid parser config."
parseExpressionConfig _ = fail "failed to parse expression"

createBooleanParser :: [String] -> Formatter -> Parser Expr
createBooleanParser (x:y:[]) (p,s) = fmap Integer (parseGivenString p *> (parseGivenString x $> 1 <|> parseGivenString y $> 0) <* parseGivenString s)
createBooleanParser _ _ = fail "Invalid boolean configuration: expected two values."

parseBooleanConfig :: Parser (Parser Expr)
parseBooleanConfig = do
    formatter <- parseGivenString "boolean" *> parseFormatters
    parseWhiteSpaces *> parseChar '[' *> parseWhiteSpaces
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
    parseWhiteSpaces *> parseGivenString "[" *> parseWhiteSpaces *> parseSepBy (parseOperatorConfig' formatters) (parseGivenString ",") <* parseGivenString "]"

createParametersConfig :: [String] -> Formatter -> Parser [String]
createParametersConfig ("name":y:[]) (p,s) = parseGivenString p *> parseWhiteSpaces *> (parseSepBy parseName (parseWhiteSpaces *> parseGivenString y *> parseWhiteSpaces) <|> pure []) <* parseWhiteSpaces <* parseGivenString s
createParametersConfig ("name":[]) (p,s) = parseGivenString p *> parseWhiteSpaces *> (parseSepBy parseName parseWhiteSpaces <|> pure []) <* parseWhiteSpaces <* parseGivenString s
createParametersConfig _ _ = fail "Invalid `parameters` configuration."

parseParametersConfig :: Parser (Parser [String])
parseParametersConfig = do
    formatters <- parseGivenString "parameters" *> parseFormatters
    parseWhiteSpaces
    content <- parseSepBy (parseStringInQuotes <|> parseConfigString) (parseGivenString "->" *> parseWhiteSpaces)
    pure $ (createParametersConfig content formatters)

parseConditionConfig :: Parser (Parser Expr)
parseConditionConfig = do
    formatters <- parseGivenString "condition" *> parseFormatters
    (parseWhiteSpaces *> parseGivenString "expression") <|> fail "Invalid `condition` configuration."
    pure $ parseExpression

createVariableConfig' :: [String] -> Parser Expr
createVariableConfig' [] = fail "Invalid `variable` configuration."
createVariableConfig' tab = fmap Var (loopedParser tab *> parseWhiteSpaces *> parseName)

createVariableConfig :: [String] -> Formatter -> Parser Expr
createVariableConfig [] _ = fail "Invalid `variable` configuration."
createVariableConfig content (p,s) = parseGivenString p *> createVariableConfig' content <* parseGivenString s

parseVariableConfig :: Parser (Parser Expr)
parseVariableConfig = do
    formatters <- parseGivenString "variable" *> parseFormatters <* parseWhiteSpaces
    table <- parseGivenString "[" *> parseSepBy (parseStringInQuotes) (parseWhiteSpaces *> parseGivenString "," <* parseWhiteSpaces) <* parseGivenString "]" <* parseWhiteSpaces
    pure $ createVariableConfig table formatters

parseCodeBlock :: (Formatter, [String]) -> ParserConfig -> Parser Expr
parseCodeBlock ((p,s),sep:l) cfg = List <$> ((parseGivenString p) *>
    parseSepBy (parseExpressionConfig cfg) (parseGivenString sep)
    <* parseGivenString s) <|> parseCodeBlock ((p,s),l) cfg

parseCodeBlockConfig :: Parser (Formatter, [String])
parseCodeBlockConfig = do
    parseGivenString "codeBlock"
    formatters <- parseFormatters
    parseWhiteSpaces
    separators <- parseGivenString "[" *>
        parseSepBy parseStringInQuotes (parseWhiteSpaces *> parseGivenString "," *> parseWhiteSpaces)
        <* parseGivenString "]"
    pure $ (formatters, separators)

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

getOrder :: [String] -> [String]
getOrder [] = []
getOrder (('[':_):xs) = "declarator" : getOrder xs
getOrder (x:xs) = x : getOrder xs

getDeclarators :: [String] -> [String]
getDeclarators [] = []
getDeclarators (('[':xs):_) = case parseTable' xs of
    Left err -> []
    Right value -> value

functionParserConfig :: (Formatter, [String], [String]) -> ParserConfig -> Parser Expr
functionParserConfig ((p,s),("name":"declarator":xs),declarators) cfg = do
    name <- parseName <* parseWhiteSpaces
    parseGivenString p *> parseWhiteSpaces *> loopedParser declarators *> parseWhiteSpaces
    parameters <- (parseParameter cfg) <* parseWhiteSpaces
    codeBlock <- (parseExpressionConfig cfg) <* parseWhiteSpaces <* parseGivenString s
    pure $ Function name parameters codeBlock
functionParserConfig ((p,s),("declarator":"name":xs),declarators) cfg = do
    parseGivenString p *> parseWhiteSpaces *> loopedParser declarators *> parseWhiteSpaces
    name <- parseName <* parseWhiteSpaces
    parameters <- (parseParameter cfg) <* parseWhiteSpaces
    codeBlock <- (parseExpressionConfig cfg) <* parseWhiteSpaces
    pure $ Function name parameters codeBlock

parseFunctionConfig :: Parser (Formatter, [String], [String])
parseFunctionConfig = do
    formatters <- parseGivenString "function" *> parseFormatters <* parseWhiteSpaces
    line <- parseSepBy (parseConfigString) (parseWhiteSpaces *> parseGivenString "->" <* parseWhiteSpaces)
    pure $ (formatters, getOrder line, getDeclarators line)

ifParserConfig :: (Formatter, [String], [String]) -> ParserConfig -> Parser Expr
ifParserConfig ((p,s),pref,suf) cfg = do
    loopedParser pref *> parseWhiteSpaces *> parseGivenString p *> parseWhiteSpaces
    condition <- (parseExpressionConfig cfg) <* parseWhiteSpaces
    fstBlock <- (parseExpressionConfig cfg) <* parseWhiteSpaces
    parseWhiteSpaces *> loopedParser suf *> parseWhiteSpaces
    sndBlock <- (parseExpressionConfig cfg) <* parseGivenString s
    pure $ If condition fstBlock sndBlock

parseIfConfig :: Parser (Formatter, [String], [String])
parseIfConfig = do
    formatters <- parseGivenString "if" *> parseFormatters <* parseWhiteSpaces
    pref <- parseGivenString "[" *>
        parseSepBy parseStringInQuotes (parseWhiteSpaces *> parseGivenString "," *> parseWhiteSpaces)
        <* parseGivenString "]"
    parseWhiteSpaces *> parseGivenString "->" *> parseWhiteSpaces
    suf <- parseGivenString "[" *>
        parseSepBy parseStringInQuotes (parseWhiteSpaces *> parseGivenString "," *> parseWhiteSpaces)
        <* parseGivenString "]"
    pure $ (formatters,pref,suf)

parseConfigTest :: IO ()
parseConfigTest = case parse "variable{prefix: \"(\", suffix: \")\"}: name -> [\"define\"]" parseVariableConfig of
    Left err -> putStrLn $ show err
    Right pa -> case parse "(a define)" pa of
        Left err -> putStrLn $ show err
        Right result -> putStrLn $ show result

parseWithConfig :: String -> ParserConfig -> IO()
parseWithConfig s pc = case parse s (parseExpressionConfig pc) of
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
                        Right code -> case parse (tab !! 6) parseIfConfig of
                            Right ifConf -> case parse (tab !! 7) parseFunctionConfig of
                                Right func -> Just $ ParserConfig bool var op cond param code ifConf func
                                Left _ -> trace ("fail function") Nothing
                            Left _ -> trace ("fail if") Nothing
                        Left _ -> trace ("fail code") Nothing
                    Left _ -> trace ("fail param") Nothing
                Left _ -> trace ("fail cond") Nothing
            Left _ -> trace ("fail op") Nothing
        Left _ -> trace ("fail var") Nothing
    Left _ -> trace ("fail bool") Nothing
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
