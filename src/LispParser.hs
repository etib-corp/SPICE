module LispParser
where

import MyParser
import Lib
import Structures

import Control.Applicative
import Data.Functor
import Structures

type Name = String

parseOperator :: Parser Expr
parseOperator = fmap Operator (parseGivenString "+" <|>
                               parseGivenString "-" <|>
                               parseGivenString "=" <|>
                               parseGivenString "/" <|>
                               parseGivenString "*" <|> fail "Failed to parse operator")

parseInteger :: Parser Expr
parseInteger = fmap Integer (parseInt <|> fail "Failed to parse Integer")

parseFloat :: Parser Expr
parseFloat = fmap Float (parseDouble <|> fail "Failed to parse Float")

parseArithmeticOp :: Parser Expr
parseArithmeticOp = ArithmeticOp
    <$> ((parseGivenString "+") <|> (parseGivenString "-") <|> (parseGivenString "*") <|> (parseGivenString "/"))
    <*> (parseWhiteSpaces *> parseExpression)
    <*> (parseWhiteSpaces *> parseExpression)

-- parseFunction :: Parser Expr
-- parseFunction = fmap Function parseString ((fmap (:) parseString) parseExpression)

parseVar :: Parser Expr
parseVar = fmap Var ((parseGivenString "define" *> parseWhiteSpaces *> parseString) <|>
                      fail "Failed to parse Variable")

parseList :: Parser Expr
parseList = fmap List ((parseGivenString "list" *> parseWhiteSpaces *>
                       parseSepBy parseExpression parseWhiteSpaces) <|>
                       fail "Failed to parse List")

parseLispExpressionTest :: Parser Expr
parseLispExpressionTest = parseGivenString "(" *> parseExpression <* parseGivenString ")"

parseExpression :: Parser Expr
parseExpression = parseVar <|> parseInteger <|> parseList
