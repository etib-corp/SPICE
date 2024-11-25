module Structures where

type Name = String

data Expr
  = Int Integer
  | Declarator Name
  | Float Double
  | Var Name
  | Operator Name
  | Call Name
  | Condition Name
  | Callable Name [Expr]
  | Statement Name
  | Function Name [Name] Expr
  | ArithmeticOp Name Expr Expr
  | List [Expr]
  | If Expr Name Expr Expr
--   | Call Name [Expr]
--   | Function Name [Name] Expr
--   | Extern Name [Name]
--   | ArithmeticOp Name Expr Expr
--   | UnaryOp Name Expr
  deriving (Eq, Ord, Show)

data AST a = Empty | Node a [AST a]

instance (Show a) => Show (AST a) where
  show Empty = ""
  show (Node a []) = show a
  show (Node a xs) = show a ++ " -> \n" ++ show xs

instance Functor AST where
  fmap f Empty = Empty
  fmap f (Node a xs) = Node (f a) (map (fmap f) xs)
