module OperatorConfig where

import MyParser
import Structures
import Formatters
import LispParser

import Control.Applicative

import Data.Functor

formatOperator :: String -> String
formatOperator "plus:" = "+"
formatOperator "minus:" = "-"
formatOperator "multiply:" = "*"
formatOperator "divide:" = "div"
formatOperator "modulo:" = "mod"
formatOperator "equal:" = "eq?"
formatOperator "assignation:" = "define"

createOperatorParser :: [String] -> Formatter -> String -> Parser Expr
createOperatorParser (op:"expression":"expression":[]) (p,s) declarator =
    ArithmeticOp <$>
    (parseGivenString p *> parseGivenString op $> formatOperator declarator)
    <*> (parseWhiteSpaces *> parseExpression)
    <*> (parseWhiteSpaces *> parseExpression)
    <* parseGivenString s
createOperatorParser ("expression":op:"expression":[]) (p,s) declarator = do
    leftExpr <- parseGivenString p *> parseExpression
    name <- parseWhiteSpaces *> parseGivenString op $> formatOperator declarator
    rightExpr <- parseWhiteSpaces *> parseExpression <* parseGivenString s
    return $ ArithmeticOp name leftExpr rightExpr
createOperatorParser ("expression":"expression":op:[]) (p,s) declarator = do
    leftExpr <- parseGivenString p *> parseExpression
    rightExpr <- parseWhiteSpaces *> parseExpression
    name <- parseWhiteSpaces *> parseGivenString op $> formatOperator declarator <* parseGivenString s
    return $ ArithmeticOp name leftExpr rightExpr
createOperatorParser t _ _ =  if (length t) >= 3 then fail ">3" else fail "<3"

parseOperatorConfig' :: Formatter -> Parser (Parser Expr)
parseOperatorConfig' f = do
    declarator <- parseWhiteSpaces *> parseDeclarator <* parseWhiteSpaces
    operatorValue <- parseSepBy (parseString' <|> parseStringInQuotes) (parseWhiteSpaces *> parseGivenString "->" <* parseWhiteSpaces)
    pure $ createOperatorParser operatorValue f declarator
        where
            parseDeclarator = parseGivenString "plus:" <|> parseGivenString "minus:" <|> parseGivenString "multiply:" <|> parseGivenString "divide:" <|> parseGivenString "modulo:" <|> parseGivenString "equal:" <|> parseGivenString "assignation:"

parseOperatorConfig :: Parser [Parser Expr]
parseOperatorConfig = do
    parseGivenString "operators"
    formatters <- parseFormatters
    parseWhiteSpaces *> parseGivenString "[" *> parseWhiteSpaces *> parseSepBy (parseOperatorConfig' formatters) (parseGivenString ",") <* parseGivenString "]"
