module OperatorConfig where

import MyParser
import Structures
import Formatters
import LispParser

import Control.Applicative

import Data.Functor

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
