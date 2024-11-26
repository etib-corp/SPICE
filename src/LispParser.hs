module LispParser
where

import MyParser
import Lib
import Structures

import Control.Applicative
import Data.Functor
import Structures

parseOperator :: Parser Expr
parseOperator = fmap Operator (parseGivenString "+" <|>
                               parseGivenString "-" <|>
                               parseGivenString "=" <|>
                               parseGivenString "/" <|>
                               parseGivenString "*" <|> fail "Failed to parse operator")

parseParenOp :: Parser ()
parseParenOp = void $ (parseWhiteSpaces *> parseGivenString "(" <* parseWhiteSpaces) <|> fail "Failed to parse Parenthesis"

parseParenCl :: Parser ()
parseParenCl = void $ (parseWhiteSpaces *> parseGivenString ")" <* parseWhiteSpaces) <|> fail "Failed to parse Parenthesis"

parseInteger :: Parser Expr
parseInteger = fmap Integer (parseInt <|> fail "Failed to parse Integer")

parseFloat :: Parser Expr
parseFloat = fmap Float (parseDouble <|> fail "Failed to parse Float")

parseArithmeticOp :: Parser Expr
parseArithmeticOp = parseParenOp *> parseArithmeticExpr <* parseParenCl

parseArithmeticExpr :: Parser Expr
parseArithmeticExpr = ArithmeticOp
    <$> (parseGivenString "+" <|> parseGivenString "-" <|> parseGivenString "*" <|> parseGivenString "/")
    <*> (parseWhiteSpaces *> parseExpression)
    <*> (parseWhiteSpaces *> parseExpression)

parseFunction :: Parser Expr
parseFunction = Function
    <$> (parseGivenString "define" *> parseParenOp *> parseName <* parseWhiteSpaces)
    <*> (parseWhiteSpaces *> (parseSepBy parseName parseWhiteSpaces <|> pure []) <* parseWhiteSpaces <* parseParenCl)
    <*> (parseWhiteSpaces *> parseExpression)

parseCallable :: Parser Expr
parseCallable = parseParenOp *> parseCallableExpr <* parseParenCl

parseCallableExpr :: Parser Expr
parseCallableExpr = Callable
    <$> parseName
    <*> (parseWhiteSpaces
        *> (parseSepBy parseExpression parseWhiteSpaces <|> pure [])
        <* parseWhiteSpaces)

parseVar :: Parser Expr
parseVar = fmap Var ((parseGivenString "define" *> parseWhiteSpaces *> parseString) <|>
                      fail "Failed to parse Variable")

parseList :: Parser Expr
parseList = parseParenOp *> parseListExpr <* parseParenCl

parseListExpr :: Parser Expr
parseListExpr = fmap List ((parseGivenString "list" *> parseWhiteSpaces *>
                       parseSepBy parseExpression parseWhiteSpaces) <|>
                       fail "Failed to parse List")

parseLispExpressionTest :: Parser Expr
parseLispExpressionTest = parseParenOp *> parseExpression <* parseParenCl

parseExpression :: Parser Expr
parseExpression = parseArithmeticOp <|> parseVar <|> parseInteger <|> parseList <|> parseFloat <|> parseOperator <|> parseFunction <|> parseCallable
