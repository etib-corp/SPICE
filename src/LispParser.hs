module LispParser
where

import MyParser
import Lib

import Control.Applicative
import Data.Functor

type Name = String

data Expr
  = Integer Int
  | Float Double
  | Var String
  | Call Name [Expr]
  | Function Name [Name] Expr
  | Extern Name [Name]
  | BinaryOp Name Expr Expr
  | UnaryOp Name Expr
  | List [Expr]
  deriving (Eq, Ord, Show)

data AST a = Node (AST a) a (AST a) | Empty deriving (Show, Eq)

parseInteger :: Parser Expr
parseInteger = fmap Integer parseInt

-- parseFloat :: Parser Expr
-- parseFloat = fmap Float parseDouble

parseVar :: Parser Expr
parseVar = fmap Var (parseGivenString "define" *> parseWhiteSpaces *> parseString)

parseList :: Parser Expr
parseList = fmap List (parseGivenString "list" *> parseWhiteSpaces *> parseSepBy parseExpression parseWhiteSpaces)

parseExpression :: Parser Expr
parseExpression = parseVar <|> parseInteger <|> parseList

parseLispExpressionTest :: Parser Expr
parseLispExpressionTest = parseGivenString "(" *> parseExpression <* parseGivenString ")"
