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

parseVar :: Parser Expr
parseVar = Var <$> parseName <|> fail "Failed to parse variable."

callableParserConfig :: (Formatter, [String], String, [String]) -> ParserConfig -> Parser Expr
callableParserConfig ((p,s),pref,sep,suf) cfg = do
    name <- parseGivenString p *> parseWhiteSpaces *> parseName
    params <- loopedParser pref *> parseSepBy (parseWhiteSpaces *> parseExpressionConfig cfg <|> parseVar) (parseWhiteSpaces *> parseGivenString sep <* parseWhiteSpaces) <* parseWhiteSpaces <* loopedParser suf
    pure $ Callable name params

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

parseSingleOperatorConfig :: Formatter -> Parser (Formatter, [String], String)
parseSingleOperatorConfig formatters = do
    declarator <- loopedParser ["plus:", "minus:", "multiply:", "divide:", "modulo:", "equal:", "assignation:", "greater:", "less:"] <* parseWhiteSpaces
    content <- parseSepBy (parseStringInQuotes <|> parseString') (parseWhiteSpaces *> parseGivenString "->" *> parseWhiteSpaces) <* parseWhiteSpaces
    pure $ (formatters, content, declarator)

parseOperatorsConfig :: Parser [(Formatter, [String], String)]
parseOperatorsConfig = do
    formatters <- parseGivenString "operators" *> parseFormatters <* parseWhiteSpaces
    parseGivenString "[" *> parseWhiteSpaces
    result <- parseSepBy (parseSingleOperatorConfig formatters) (parseWhiteSpaces *> parseGivenString "," *> parseWhiteSpaces) <* parseGivenString "]" <* parseWhiteSpaces
    pure result

useOps :: [(Formatter,[String], String)] -> ParserConfig -> Parser Expr
useOps [] _ = fail "No operators found."
useOps (x:xs) cfg = operatorParserConfig x cfg <|> useOps xs cfg

parseVariabl :: (Formatter,[String]) -> ParserConfig -> Parser Expr
parseVariabl ((p,s),declarator) cfg@(ParserConfig _ _ pops _ _ _ _ _ _) = do
    parseGivenString p *> loopedParser declarator *> parseWhiteSpaces *> (useOps pops cfg ) <* parseGivenString s

parseCondition' :: Formatter -> ParserConfig -> Parser Expr
parseCondition' (p,s) cfg = parseGivenString p *> (parseExpressionConfig cfg) <* parseGivenString s

parseCodeBlock :: (Formatter, [String]) -> ParserConfig -> Parser Expr
parseCodeBlock ((p,s),l) cfg@(ParserConfig pbool pvar pops _ ppar cb ifconf func call) = List <$> ((parseGivenString p) *>
    parseSepBy (parseWhiteSpaces *> (callableParserConfig call cfg <|> parseExpressionConfig cfg <|> parseVar)) (parseWhiteSpaces *> loopedParser l <* parseWhiteSpaces)
    <* parseGivenString s)

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
functionParserConfig ((p,s),("name":"declarator":xs),declarators) cfg@(ParserConfig _ _ _ pcond _ cb _ _ _) = do
    name <- parseName <* parseWhiteSpaces
    parseGivenString p *> parseWhiteSpaces *> loopedParser declarators *> parseWhiteSpaces
    parameters <- (parseParameter cfg) <* parseWhiteSpaces
    codeBlock <- (parseCodeBlock cb cfg) <* parseWhiteSpaces <* parseGivenString s
    pure $ Function name parameters codeBlock
functionParserConfig ((p,s),("declarator":"name":xs),declarators) cfg@(ParserConfig _ _ _ pcond _ cb _ _ _) = do
    parseGivenString p *> parseWhiteSpaces *> loopedParser declarators *> parseWhiteSpaces
    name <- parseName <* parseWhiteSpaces
    parameters <- (parseParameter cfg) <* parseWhiteSpaces
    codeBlock <- (parseCodeBlock cb cfg) <* parseWhiteSpaces
    pure $ Function name parameters codeBlock

parseFunctionConfig :: Parser (Formatter, [String], [String])
parseFunctionConfig = do
    formatters <- parseGivenString "function" *> parseFormatters <* parseWhiteSpaces
    line <- parseSepBy (parseConfigString) (parseWhiteSpaces *> parseGivenString "->" <* parseWhiteSpaces)
    pure $ (formatters, getOrder line, getDeclarators line)

ifParserConfig :: (Formatter, [String], [String]) -> ParserConfig -> Parser Expr
ifParserConfig ((p,s),decl,suf) cfg@(ParserConfig _ _ _ pcond _ cb _ _ _) = do
    parseGivenString p *> parseWhiteSpaces *> loopedParser decl *> parseWhiteSpaces
    condition <- (parseCondition' pcond cfg) <* parseWhiteSpaces
    fstBlock <- (parseCodeBlock cb cfg) <* parseWhiteSpaces
    parseWhiteSpaces *> loopedParser suf *> parseWhiteSpaces
    sndBlock <- (parseCodeBlock cb cfg) <* parseGivenString s
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
