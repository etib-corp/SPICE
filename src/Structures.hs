module Structures where

type Name = String

data Expr
  = Integer Int                 -- Parsed
  | Declarator Name             -- DONT
  | Float Double                -- Parsed
  | Var Name                    -- Parsed, need to be improved
  | Operator Name               -- Parsed
  | Call Name                   -- DONT
  | Condition Name              -- DONT
  | Callable Name [Expr]        -- Parsed, need to be improved
  | Statement Name              -- DONT
  | Function Name [Name] Expr   -- Parsed
  | ArithmeticOp Name Expr Expr -- Parsed
  | List [Expr]                 -- Parsed
  | If Expr Name Expr Expr      -- Parsed
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
