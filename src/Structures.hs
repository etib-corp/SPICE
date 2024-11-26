module Structures where

import Control.Monad
import Control.Applicative
import Control.Monad

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
  -- | Call Name [Expr]
  -- | Function Name [Name] Expr
  -- | Extern Name [Name]
  -- | ArithmeticOp Name Expr Expr
  -- | UnaryOp Name Expr
  deriving (Eq, Ord)

instance Show Expr where
  show (Integer i) = show i
  show (Float f) = show f
  show (Var v) = v
  show (Operator o) = o
  show (List l) = show l
  show (ArithmeticOp o e1 e2) = show e1 ++ " " ++ show o ++ " " ++ show e2
  show (Function n args e) = n ++ "(" ++ unwords args ++ ") = " ++ show e
  show (If c t e1 e2) = "if " ++ show c ++ " then " ++ show t ++ " else " ++ show e1 ++ " end"
  show (Callable n args) = n ++ "(" ++ unwords (map show args) ++ ")"

data AST a = Empty | Node a [AST a]

instance (Show a) => Show (AST a) where
  show Empty = ""
  show (Node a []) = show a
  show (Node a xs) = show a ++ " -> \n" ++ show xs

instance Functor AST where
  fmap f Empty = Empty
  fmap f (Node a xs) = Node (f a) (map (fmap f) xs)

data Env = Env { variables :: [(Expr, Expr)] } deriving (Show)

instance Semigroup Env where
  Env xs <> Env ys = Env $ xs ++ ys

