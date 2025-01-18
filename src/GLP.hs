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

import Control.Applicative

import Debug.Trace

import Data.Functor
import Data.Char

parseExpressionConfig :: ParserConfig -> Parser Expr
parseExpressionConfig pcfg@(ParserConfig pbool pvar pops _ ppar cb ifconf func) =
    (functionParserConfig func pcfg)
    <|> (ifParserConfig ifconf pcfg)
    <|> (useOps pops pcfg)
    <|> pbool
    <|> parseInteger
    <|> (parseCodeBlock cb pcfg)
    <|> (parseVariabl pvar)
parseExpressionConfig NullConfig = fail "Invalid parser config."
parseExpressionConfig _ = fail "failed to parse expression"

parseVar :: Parser Expr
parseVar = Var <$> parseName <|> fail "Failed to parse variable."

operatorParserConfig :: (Formatter, [String], String) -> ParserConfig -> Parser Expr
operatorParserConfig ((p,s),("expression":op:"expression":[]), declarator) cfg = do
    left <- parseGivenString p *>  (parseVar <|> parseInteger <|> parseExpressionConfig cfg) <* parseWhiteSpaces
    operator <- (parseGivenString op $> formatOperator declarator) <* parseWhiteSpaces
    right <- (parseVar <|> parseInteger <|> parseExpressionConfig cfg) <* parseWhiteSpaces <* parseGivenString s
    pure $ ArithmeticOp operator left right
operatorParserConfig ((p,s),("expression":"expression":op:[]), declarator) cfg = do
    left <- parseGivenString p *>  (parseVar <|> parseInteger <|> parseExpressionConfig cfg) <* parseWhiteSpaces
    right <- (parseVar <|> parseInteger <|> parseExpressionConfig cfg) <* parseWhiteSpaces
    operator <- (parseGivenString op $> formatOperator declarator) <* parseWhiteSpaces <* parseGivenString s
    pure $ ArithmeticOp operator left right
operatorParserConfig ((p,s),(op:"expression":"expression":[]), declarator) cfg = do
    operator <- parseGivenString p *> (parseGivenString op $> formatOperator declarator) <* parseWhiteSpaces
    left <- (parseVar <|> parseInteger <|> parseExpressionConfig cfg) <* parseWhiteSpaces
    right <- (parseVar <|> parseInteger <|> parseExpressionConfig cfg) <* parseWhiteSpaces <* parseGivenString s
    pure $ ArithmeticOp operator left right
operatorParserConfig (f, t, d) _ = fail "Failed to parse operator"

parseSingleOperatorConfig :: Formatter -> Parser (Formatter, [String], String)
parseSingleOperatorConfig formatters = do
    declarator <- loopedParser ["plus:", "minus:", "multiply:", "divide:", "modulo:", "equal:", "assignation:"] <* parseWhiteSpaces
    content <- parseSepBy (parseStringInQuotes <|> parseString') (parseWhiteSpaces *> parseGivenString "->" *> parseWhiteSpaces) <* parseWhiteSpaces
    pure $ (formatters, content, declarator)

parseOperatorsConfig :: Parser [(Formatter, [String], String)]
parseOperatorsConfig = do
    formatters <- parseGivenString "operators" *> parseFormatters <* parseWhiteSpaces <*  parseGivenString "[" <* parseWhiteSpaces
    result <- parseSepBy (parseSingleOperatorConfig formatters) (parseWhiteSpaces *> parseGivenString "," *> parseWhiteSpaces) <* parseGivenString "]" <* parseWhiteSpaces
    pure result

testParseOperatorsConfig :: Parser [String]
testParseOperatorsConfig = do
    content <- parseSepBy (parseStringInQuotes <|> parseString') (parseWhiteSpaces *> parseGivenString "->" *> parseWhiteSpaces) <* parseWhiteSpaces
    pure content

useOps :: [(Formatter,[String], String)] -> ParserConfig -> Parser Expr
useOps [] _ = fail "No operators found."
useOps (x:xs) cfg = operatorParserConfig x cfg <|> useOps xs cfg

parseVariabl :: (Formatter,[String]) -> Parser Expr
parseVariabl ((p,s),_) =  Var <$> ((parseGivenString p) *> parseName <* (parseGivenString s))

parseCondition' :: Formatter -> ParserConfig -> Parser Expr
parseCondition' (p,s) cfg = parseGivenString p *> (parseExpressionConfig cfg) <* parseGivenString s

parseCodeBlock :: (Formatter, [String]) -> ParserConfig -> Parser Expr
parseCodeBlock ((p,s),sep:l) cfg = List <$> ((parseGivenString p) *>
    parseSepBy (parseWhiteSpaces *>(parseExpressionConfig cfg)) (parseWhiteSpaces *> parseGivenString sep <* parseWhiteSpaces)
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
ifParserConfig ((p,s),pref,suf) cfg@(ParserConfig _ _ _ pcond _ _ _ _) = do
    loopedParser pref *> parseWhiteSpaces *> parseGivenString p *> parseWhiteSpaces
    condition <- (parseCondition' pcond cfg) <* parseWhiteSpaces
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

parseSyntaxConfiguration :: [String] -> Maybe ParserConfig
parseSyntaxConfiguration tab | length tab == 8 = case parse (tab !! 0) parseBooleanConfig of
    Right bool -> case parse (tab !! 1) parseVariableConfig of
        Right var -> case parse (tab !! 2) parseOperatorsConfig of
            Right op -> case parse (tab !! 3) parseConditionConfig of
                Right cond -> case parse (tab !! 4) parseParametersConfig of
                    Right param -> case parse (tab !! 5) parseCodeBlockConfig of
                        Right code -> case parse (tab !! 6) parseIfConfig of
                            Right ifConf -> case parse (tab !! 7) parseFunctionConfig of
                                Right func -> Just $ ParserConfig bool var op cond param code ifConf func
                                Left _ -> Nothing
                            Left _ -> Nothing
                        Left _ -> Nothing
                    Left _ -> Nothing
                Left _ -> Nothing
            Left _ -> Nothing
        Left _ -> Nothing
    Left _ -> Nothing
                            | otherwise = Nothing
