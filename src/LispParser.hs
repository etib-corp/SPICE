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
parseOperatorLisp :: Parser Expr
parseOperatorLisp = fmap Operator (parseGivenString "+" <|>
                               parseGivenString "-" <|>
                               parseGivenString "=" <|>
                               parseGivenString "/" <|>
                               parseGivenString "*" <|>
                               fail "Failed to parse operator")


-- | Parses a lisp builtin variable and returns it as a generic Expression.
parseBooleanLisp :: Parser Expr
parseBooleanLisp = fmap Integer (parseGivenString "#t" $> 1 <|> parseGivenString "#f" $> 0 <|> fail "Failed to parse boolean. ")

-- | Parses left parenthesis with arounding whitespaces.
parseLeftParenthesis :: Parser ()
parseLeftParenthesis = void $ (parseWhiteSpaces *> parseGivenString "(" <* parseWhiteSpaces)

-- | Parses right parenthesis with arounding whitespaces.
parseRightParenthesis :: Parser ()
parseRightParenthesis = void $ (parseWhiteSpaces *> parseGivenString ")" <* parseWhiteSpaces)

-- | Parses a lisp integer and returns it as a generic Expression.
parseInteger :: Parser Expr
parseInteger = fmap Integer (parseInt <|> fail "Failed to parse Integer")

-- | Parses a lisp float and returns it as a generic Expression.
parseFloatLisp :: Parser Expr
parseFloatLisp = fmap Float (parseDouble <|> fail "Failed to parse Float")

-- | Parses a lisp arithmetic operation and returns it as a generic Expression.
parseArithmeticOpLisp :: Parser Expr
parseArithmeticOpLisp = parseLeftParenthesis *> parseArithmeticExpr <* parseRightParenthesis

-- | Parses a lisp arithmetic expression and returns it as a generic Expression.
parseArithmeticExpr :: Parser Expr
parseArithmeticExpr = ArithmeticOp
    <$> ((parseGivenString "+" <|> parseGivenString "-" <|> parseGivenString "*"
        <|> parseGivenString "/" <|> parseGivenString "eq?" <|> parseGivenString "define" <|> parseGivenString "div" <|> parseGivenString "mod" <|> parseGivenString "<") <|> fail "Invalid declaration. Expected: [+, -, *, /, eq?, define].")
    <*> parseExpr
    <*> parseExpr
    where
        parseExpr = ((parseWhiteSpaces *> (parseExpression <|> parseVarLisp)) <|> fail ("Invalid call or expression. Expected a variable name or a Lisp expression."))

-- | Parses a lisp function and returns it as a generic Expression.
parseFunctionLisp :: Parser Expr
parseFunctionLisp = Function
    <$> (parseLeftParenthesis *> parseGivenString "define" *> parseLeftParenthesis *> parseName <* parseWhiteSpaces)
    <*> (parseWhiteSpaces *> (parseSepBy parseName parseWhiteSpaces <|> pure []) <* parseWhiteSpaces <* parseRightParenthesis)
    <*> (parseWhiteSpaces *> parseExpression <* parseRightParenthesis)

-- | Parses a lisp callable object and returns it as a generic Expression.
parseCallableLisp :: Parser Expr
parseCallableLisp = parseLeftParenthesis *> parseCallableExpr <* parseRightParenthesis

-- | Parses a lisp callable expression and returns it as a generic Expression.
parseCallableExpr :: Parser Expr
parseCallableExpr = Callable
    <$> parseName
    <*> (parseWhiteSpaces
        *> (parseSepBy (parseExpression <|> parseVarLisp) parseWhiteSpaces <|> pure [])
        <* parseWhiteSpaces)

-- | Parses a lisp variable and returns it as a generic Expression.
parseVarLisp :: Parser Expr
parseVarLisp = fmap Var (parseName <|> fail "Invalid variable name")

-- | Parses a lisp list and returns it as a generic Expression.
parseListLisp :: Parser Expr
parseListLisp = parseLeftParenthesis *> parseListExpr <* parseRightParenthesis

-- | Parses a lisp list expression and returns it as a generic Expression.
parseListExpr :: Parser Expr
parseListExpr = fmap List ((parseGivenString "list" *> parseWhiteSpaces *>
                       parseSepBy parseExpression parseWhiteSpaces) <|>
                       fail "Failed to parse List")

-- | Parses a lisp list if and returns it as a generic Expression.
parseIfLisp :: Parser Expr
parseIfLisp = If <$> parseStart <*> (parseWhiteSpaces *> parseExpression) <*> parseExpr
    where
        parseStart = (parseLeftParenthesis *> parseGivenString "if" *> parseWhiteSpaces *> parseExpression)
        parseExpr = (parseWhiteSpaces *> parseExpression <* parseRightParenthesis)

-- | Parses a lisp expression, only used for debugging.
-- parseLispExpressionTest :: Parser Expr
-- parseLispExpressionTest = parseManyUntil (parseWhiteSpaces *> parseExpression *> parseWhiteSpaces)

-- | Parses a lisp expression and returns it as a generic Expression.
parseExpression :: Parser Expr
parseExpression = parseFunctionLisp <|> parseArithmeticOpLisp <|> parseBooleanLisp <|> parseInteger <|>
                  parseListLisp <|> parseFloatLisp <|> parseOperatorLisp <|> (parseIfLisp <|>
                  parseCallableLisp) <|> parseVarLisp
