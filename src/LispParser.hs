-- | This module provides a global Lisp parser with the main languages's features.
-- This module uses the parser combinator that we implemented.
module LispParser where

import MyParser
import Lib
import Structures

import Control.Applicative
import Data.Functor
import Structures

-- | Parses only basic operators like (+, -, *, /, =) and returns it as a generic Expression.
parseOperator :: Parser Expr
parseOperator = fmap Operator (parseGivenString "+" <|>
                               parseGivenString "-" <|>
                               parseGivenString "=" <|>
                               parseGivenString "/" <|>
                               parseGivenString "*" <|> fail "Failed to parse operator")

-- | Parses left parenthesis with arounding whitespaces.
parseLeftParenthesis :: Parser ()
parseLeftParenthesis = void $ (parseWhiteSpaces *> parseGivenString "(" <* parseWhiteSpaces) <|> fail "Failed to parse Parenthesis"

-- | Parses right parenthesis with arounding whitespaces.
parseRightParenthesis :: Parser ()
parseRightParenthesis = void $ (parseWhiteSpaces *> parseGivenString ")" <* parseWhiteSpaces) <|> fail "Failed to parse Parenthesis"

-- | Parses a lisp integer and returns it as a generic Expression.
parseInteger :: Parser Expr
parseInteger = fmap Integer (parseInt <|> fail "Failed to parse Integer")

-- | Parses a lisp float and returns it as a generic Expression.
parseFloat :: Parser Expr
parseFloat = fmap Float (parseDouble <|> fail "Failed to parse Float")

-- | Parses a lisp arithmetic operation and returns it as a generic Expression.
parseArithmeticOp :: Parser Expr
parseArithmeticOp = parseLeftParenthesis *> parseArithmeticExpr <* parseRightParenthesis

-- | Parses a lisp arithmetic expression and returns it as a generic Expression.
parseArithmeticExpr :: Parser Expr
parseArithmeticExpr = ArithmeticOp
    <$> ((parseGivenString "+" <|> parseGivenString "-" <|> parseGivenString "*"
        <|> parseGivenString "/" <|> parseGivenString "define") <|> fail "Invalid declaration")
    <*> (parseWhiteSpaces *> (parseExpression <|> parseVar))
    <*> (parseWhiteSpaces *> (parseExpression <|> parseVar))

-- | Parses a lisp function and returns it as a generic Expression.
parseFunction :: Parser Expr
parseFunction = Function
    <$> (parseLeftParenthesis *> parseGivenString "define" *> parseLeftParenthesis *> parseName <* parseWhiteSpaces)
    <*> (parseWhiteSpaces *> (parseSepBy parseName parseWhiteSpaces <|> pure []) <* parseWhiteSpaces <* parseRightParenthesis)
    <*> (parseWhiteSpaces *> parseExpression <* parseRightParenthesis)

-- | Parses a lisp callable object and returns it as a generic Expression.
parseCallable :: Parser Expr
parseCallable = parseLeftParenthesis *> parseCallableExpr <* parseRightParenthesis

-- | Parses a lisp callable expression and returns it as a generic Expression.
parseCallableExpr :: Parser Expr
parseCallableExpr = Callable
    <$> parseName
    <*> (parseWhiteSpaces
        *> (parseSepBy (parseExpression <|> parseVar) parseWhiteSpaces <|> pure [])
        <* parseWhiteSpaces)

-- | Parses a lisp variable and returns it as a generic Expression.
parseVar :: Parser Expr
parseVar = fmap Var (parseName <|> fail "Invalid variable name")

-- | Parses a lisp list and returns it as a generic Expression.
parseList :: Parser Expr
parseList = parseLeftParenthesis *> parseListExpr <* parseRightParenthesis

-- | Parses a lisp list expression and returns it as a generic Expression.
parseListExpr :: Parser Expr
parseListExpr = fmap List ((parseGivenString "list" *> parseWhiteSpaces *>
                       parseSepBy parseExpression parseWhiteSpaces) <|>
                       fail "Failed to parse List")

-- | Parses a lisp list if and returns it as a generic Expression.
parseIf :: Parser Expr
parseIf = If <$> parseStart <*> (parseWhiteSpaces *> parseString) <*> parseExpr <*> parseExpr
    where
        parseStart = (parseLeftParenthesis *> parseGivenString "if" *> parseWhiteSpaces *> parseExpression)
        parseExpr = (parseWhiteSpaces *> parseExpression <* parseRightParenthesis)

-- | Parses a lisp expression, only used for debugging.
-- parseLispExpressionTest :: Parser Expr
-- parseLispExpressionTest = parseManyUntil (parseWhiteSpaces *> parseExpression *> parseWhiteSpaces)

-- | Parses a lisp expression and returns it as a generic Expression.
parseExpression :: Parser Expr
parseExpression = parseArithmeticOp <|> parseInteger <|>
                  parseList <|> parseFloat <|> parseOperator <|>
                  parseFunction <|> parseCallable <|> parseIf
