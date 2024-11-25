module LispParser
where

import MyParser
import Lib

import Control.Applicative
import Data.Functor
import Structures

type Name = String

parseInteger :: Parser Expr
parseInteger = fmap Integer parseInt

parseFloat :: Parser Expr
parseFloat = fmap Float parseDouble

parseArithmeticOp :: Parser Expr
parseArithmeticOp = ArithmeticOp
    <$> ((parseGivenString "+") <|> (parseGivenString "-") <|> (parseGivenString "*") <|> (parseGivenString "/"))
    <*> (parseWhiteSpaces *> parseExpression)
    <*> (parseWhiteSpaces *> parseExpression)

-- parseFunction :: Parser Expr
-- parseFunction = fmap Function parseString ((fmap (:) parseString) parseExpression)

parseVar :: Parser Expr
parseVar = fmap Var (parseGivenString "define" *> parseWhiteSpaces *> parseString)

parseList :: Parser Expr
parseList = fmap List (parseGivenString "(" *> parseGivenString "list" *>
    parseWhiteSpaces *> parseSepBy parseExpression parseWhiteSpaces
    <* parseGivenString ")")

parseExpression :: Parser Expr
parseExpression = parseVar <|> parseInteger <|> parseFloat <|> parseList <|> parseArithmeticOp

parseLispExpressionTest :: Parser Expr
parseLispExpressionTest = parseGivenString "(" *> parseExpression <* parseGivenString ")"
