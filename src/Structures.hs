module Structures where

type Name = String

data Expr
  = Int Integer
  | Float Double
  | Var Name
  | Operator Name
  | Callable Name
  | Function Name [Name] Expr
  | ArithmeticOp Name Expr Expr
  | List [Expr]
--   | Call Name [Expr]
--   | Function Name [Name] Expr
--   | Extern Name [Name]
--   | ArithmeticOp Name Expr Expr
--   | UnaryOp Name Expr
  deriving (Eq, Ord, Show)

data AST a = Node (AST a) a (AST a) | Empty deriving (Show, Eq)


