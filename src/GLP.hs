module GLP where

import MyParser
import Structures
import Files
import Lib
import LispParser
import Formatters
import GLPUtils

import BooleanConfig
import OperatorConfig
import ParametersConfig
import ConditionConfig
import VariableConfig
import FunctionConfig
import CallableConfig

import Control.Applicative

import Debug.Trace

import Data.Functor
import Data.Char

-- Parse an Expression with a given configuration
parseExpressionConfig :: ParserConfig -> Parser Expr
parseExpressionConfig pcfg@(ParserConfig pbool pvar pops _ ppar cb ifconf func call) =
    (ifParserConfig ifconf pcfg)
    <|> (useOps pops pcfg)
    <|> (functionParserConfig func pcfg)
    <|> pbool
    <|> parseInteger
    <|> (callableParserConfig call pcfg)
    <|> (parseVariabl pvar pcfg)
parseExpressionConfig NullConfig = fail "Invalid parser config."
parseExpressionConfig _ = fail "failed to parse expression"

-- Parse a Variable with the given format: \b[a-zA-Z_][a-zA-Z0-9_]*\b
parseVar :: Parser Expr
parseVar = Var <$> parseName <|> fail "Failed to parse variable."

-- Parse a Callable Expression with a given configuration and its formatters and separators.
callableParserConfig :: (Formatter, [String], String, [String]) -> ParserConfig -> Parser Expr
callableParserConfig ((p,s),pref,sep,suf) cfg = Callable <$> name <*> params
    where
        name = parseGivenString p *> parseWhiteSpaces *> parseName
        params = loopedParser pref *> parseSepBy (parseWhiteSpaces *> parseExpressionConfig cfg <|> parseVar) (parseWhiteSpaces *> parseGivenString sep <* parseWhiteSpaces) <* parseWhiteSpaces <* loopedParser suf

-- Parse a single operator with the given configuration and its formatters and separators. It returns a Parser Expr.
operatorParserConfig :: (Formatter, [String], String) -> ParserConfig -> Parser Expr
operatorParserConfig ((p,s),("expression":op:"expression":[]), declarator) cfg@(ParserConfig pbool pvar pops _ ppar cb ifconf func call) = do
    left <- parseGivenString p *>  (callableParserConfig call cfg <|> parseVar <|> parseInteger <|> parseExpressionConfig cfg) <* parseWhiteSpaces
    operator <- (parseGivenString op $> formatOperator declarator) <* parseWhiteSpaces
    right <- (callableParserConfig call cfg <|> parseVar <|> parseInteger <|> parseExpressionConfig cfg) <* parseWhiteSpaces <* parseGivenString s
    pure $ ArithmeticOp operator left right
operatorParserConfig ((p,s),("expression":"expression":op:[]), declarator) cfg@(ParserConfig pbool pvar pops _ ppar cb ifconf func call) = do
    left <- parseGivenString p *>  (callableParserConfig call cfg <|> parseVar <|> parseInteger <|> parseExpressionConfig cfg) <* parseWhiteSpaces
    right <- (callableParserConfig call cfg <|> parseVar <|> parseInteger <|> parseExpressionConfig cfg) <* parseWhiteSpaces
    operator <- (parseGivenString op $> formatOperator declarator) <* parseWhiteSpaces <* parseGivenString s
    pure $ ArithmeticOp operator left right
operatorParserConfig ((p,s),(op:"expression":"expression":[]), declarator) cfg@(ParserConfig pbool pvar pops _ ppar cb ifconf func call) = do
    operator <- parseGivenString p *> (parseGivenString op $> formatOperator declarator) <* parseWhiteSpaces
    left <- (callableParserConfig call cfg <|> parseVar <|> parseInteger <|> parseExpressionConfig cfg) <* parseWhiteSpaces
    right <- (callableParserConfig call cfg <|> parseVar <|> parseInteger <|> parseExpressionConfig cfg) <* parseWhiteSpaces <* parseGivenString s
    pure $ ArithmeticOp operator left right
operatorParserConfig (f, t, d) _ = fail "Failed to parse operator"

-- Parse a single operator configuration and returns it as a tuple with the formatters, the content and the declarator.
parseSingleOperatorConfig :: Formatter -> Parser (Formatter, [String], String)
parseSingleOperatorConfig formatters = do
    declarator <- loopedParser ["plus:", "minus:", "multiply:", "divide:", "modulo:", "equal:", "assignation:", "greater:", "less:"] <* parseWhiteSpaces
    content <- parseSepBy (parseStringInQuotes <|> parseString') (parseWhiteSpaces *> parseGivenString "->" *> parseWhiteSpaces) <* parseWhiteSpaces
    pure $ (formatters, content, declarator)

-- Parse operators configuration and returns a list of tuples with the formatters, the content and the declarator.
parseOperatorsConfig :: Parser [(Formatter, [String], String)]
parseOperatorsConfig = do
    formatters <- parseGivenString "operators" *> parseFormatters <* parseWhiteSpaces
    parseGivenString "[" *> parseWhiteSpaces
    result <- parseSepBy (parseSingleOperatorConfig formatters) (parseWhiteSpaces *> parseGivenString "," *> parseWhiteSpaces) <* parseGivenString "]" <* parseWhiteSpaces
    pure result

-- Uses a list of operators configuration to parse with the given configuration.
useOps :: [(Formatter,[String], String)] -> ParserConfig -> Parser Expr
useOps [] _ = fail "No operators found."
useOps (x:xs) cfg = operatorParserConfig x cfg <|> useOps xs cfg

-- Parse a Variable with the given configuration and its formatters and separators.
parseVariabl :: (Formatter,[String]) -> ParserConfig -> Parser Expr
parseVariabl ((p,s),declarator) cfg@(ParserConfig _ _ pops _ _ _ _ _ _) = parseGivenString p *> loopedParser declarator *> parseWhiteSpaces *> (useOps pops cfg ) <* parseGivenString s

-- Parse a Condition with the given configuration and its formatters and separators.
parseCondition' :: Formatter -> ParserConfig -> Parser Expr
parseCondition' (p,s) cfg = parseGivenString p *> (parseExpressionConfig cfg) <* parseGivenString s


-- Parse a CodeBlock using the given configuration and its formatters and separators.
parseCodeBlock :: (Formatter, [String]) -> ParserConfig -> Parser Expr
parseCodeBlock ((p,s),l) cfg@(ParserConfig pbool pvar pops _ ppar cb ifconf func call) = List <$> ((parseGivenString p) *>
    parseSepBy (parseWhiteSpaces *> (callableParserConfig call cfg <|>
    parseExpressionConfig cfg <|> parseVar)) (parseWhiteSpacesUntil l *> loopedParser l <* parseWhiteSpacesWith l)
    <* parseGivenString s)

-- Parse a CodeBlock configuration and returns it as a tuple with the formatters and the separators.
parseCodeBlockConfig :: Parser (Formatter, [String])
parseCodeBlockConfig = (,) <$> formatters <*> separators
    where
        formatters = parseGivenString "codeBlock" *> parseFormatters <* parseWhiteSpaces
        separators = parseGivenString "[" *> parseSepBy parseStringInQuotes (parseWhiteSpaces *> parseGivenString "," *> parseWhiteSpaces) <* parseGivenString "]"

-- Use a tuple with formatters and separators to parse a function using a given configuration.
functionParserConfig :: (Formatter, [String], [String]) -> ParserConfig -> Parser Expr
functionParserConfig ((p,s),("name":"declarator":xs),declarators) cfg@(ParserConfig _ _ _ pcond _ cb _ _ _) = Function <$> name <*> parameters <*> codeBlock
    where
        name = parseName <* parseWhiteSpaces <* parseGivenString p <* parseWhiteSpaces <* loopedParser declarators <* parseWhiteSpaces
        parameters = (parseParameter cfg) <* parseWhiteSpaces
        codeBlock = (parseCodeBlock cb cfg) <* parseWhiteSpaces <* parseGivenString s
functionParserConfig ((p,s),("declarator":"name":xs),declarators) cfg@(ParserConfig _ _ _ pcond _ cb _ _ _) = do
    parseGivenString p *> parseWhiteSpaces *> loopedParser declarators *> parseWhiteSpaces
    name <- parseName <* parseWhiteSpaces
    parameters <- (parseParameter cfg) <* parseWhiteSpaces
    codeBlock <- (parseCodeBlock cb cfg) <* parseWhiteSpaces
    pure $ Function name parameters codeBlock

-- Parse a function configuration and returns it as a tuple with the formatters, the order and the declarators.
parseFunctionConfig :: Parser (Formatter, [String], [String])
parseFunctionConfig = do
    formatters <- parseGivenString "function" *> parseFormatters <* parseWhiteSpaces
    line <- parseSepBy (parseConfigString) (parseWhiteSpaces *> parseGivenString "->" <* parseWhiteSpaces)
    pure $ (formatters, getOrder line, getDeclarators line)

-- Use a tuple with formatters and separators to parse an if statement using a given configuration.
ifParserConfig :: (Formatter, [String], [String]) -> ParserConfig -> Parser Expr
ifParserConfig ((p,s),decl,suf) cfg@(ParserConfig _ _ _ pcond _ cb _ _ _) = If <$> condition <*> fstBlock <*> sndBlock
    where
        condition = parseGivenString p *> parseWhiteSpaces *> loopedParser decl *> parseWhiteSpaces *> (parseCondition' pcond cfg) <* parseWhiteSpaces
        fstBlock = (parseCodeBlock cb cfg) <* parseWhiteSpaces <* loopedParser suf <* parseWhiteSpaces
        sndBlock = (parseCodeBlock cb cfg) <* parseGivenString s

-- Parse an if configuration and returns it as a tuple with the formatters and the separators.
parseIfConfig :: Parser (Formatter, [String], [String])
parseIfConfig = (,,) <$> formatters <*> pref <*> suf
    where
        formatters = parseGivenString "if" *> parseFormatters <* parseWhiteSpaces
        pref = parseGivenString "[" *> parseSepBy parseStringInQuotes (parseWhiteSpaces *> parseGivenString "," *> parseWhiteSpaces) <*
            parseGivenString "]" <* parseWhiteSpaces <* parseGivenString "->" <* parseWhiteSpaces
        suf = parseGivenString "[" *> parseSepBy parseStringInQuotes (parseWhiteSpaces *> parseGivenString "," *> parseWhiteSpaces) <* parseGivenString "]"

-- Parse a whole syntax configuration given as a list of strings, and returns a ParserConfig if it is valid or Nothing otherwise.
parseSyntaxConfiguration :: [String] -> Maybe ParserConfig
parseSyntaxConfiguration tab | length tab == 9 = case parse (tab !! 0) parseBooleanConfig of
    Right bool -> case parse (tab !! 1) parseVariableConfig of
        Right var -> case parse (tab !! 2) parseOperatorsConfig of
            Right op -> case parse (tab !! 3) parseConditionConfig of
                Right cond -> case parse (tab !! 4) parseParametersConfig of
                    Right param -> case parse (tab !! 5) parseCodeBlockConfig of
                        Right code -> case parse (tab !! 6) parseIfConfig of
                            Right ifConf -> case parse (tab !! 7) parseFunctionConfig of
                                Right func -> case parse (tab !! 8) parseCallableConfig of
                                    Right call -> Just $ ParserConfig bool var op cond param code ifConf func call
                                    Left _ -> Nothing
                                Left _ -> Nothing
                            Left _ -> Nothing
                        Left _ -> Nothing
                    Left _ -> Nothing
                Left _ -> Nothing
            Left _ -> Nothing
        Left _ -> Nothing
    Left _ -> Nothing
                            | otherwise = Nothing
