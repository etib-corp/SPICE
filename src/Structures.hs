module Structures where

import MyParser

import Control.Monad
import Control.Applicative
import Control.Monad

import Data.List (nubBy)
import Data.Fixed (mod')

type Name = String

type Formatter = (String, String)

data Expr
  = Integer Int
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
  | If Expr Expr Expr
  deriving (Eq, Ord)

data ParserConfig = ParserConfig {
    parseBoolean' :: Parser Expr
  , parseVariable :: Parser Expr
  , parserOperator :: [(Formatter, [String], String)]
  , parseCondition :: Parser Expr
  , parseParameter :: Parser [String]
  , codeBlockConfig :: (Formatter, [String])
  , parseIf :: (Formatter,[String],[String])
  , parseFunction :: (Formatter, [String], [String])
  } | NullConfig

instance Num Expr where
  (Integer i) + (Integer j) = Integer (i + j)
  (Float i) + (Float j) = Float (i + j)
  (Integer i) + (Float j) = Float (fromIntegral i + j)
  (Float i) + (Integer j) = Float (i + fromIntegral j)
  (Integer i) - (Integer j) = Integer (i - j)
  (Float i) - (Float j) = Float (i - j)
  (Integer i) - (Float j) = Float (fromIntegral i - j)
  (Float i) - (Integer j) = Float (i - fromIntegral j)
  (Integer i) * (Integer j) = Integer (i * j)
  (Float i) * (Float j) = Float (i * j)
  (Integer i) * (Float j) = Float (fromIntegral i * j)
  (Float i) * (Integer j) = Float (i * fromIntegral j)
  abs (Integer i) = Integer (abs i)
  abs (Float f) = Float (abs f)
  signum (Integer i) = Integer (signum i)
  signum (Float f) = Float (signum f)
  fromInteger i = Integer (fromIntegral i)

instance Fractional Expr where
  (Integer i) / (Integer j) = Float (fromIntegral i / fromIntegral j)
  (Float i) / (Float j) = Float (i / j)
  (Integer i) / (Float j) = Float (fromIntegral i / j)
  (Float i) / (Integer j) = Float (i / fromIntegral j)
  fromRational r = Float (fromRational r)

instance Enum Expr where
  toEnum i = Integer i
  fromEnum (Integer i) = i
  fromEnum (Float f) = truncate f

instance Real Expr where
  toRational (Integer i) = toRational i
  toRational (Float f) = toRational f

instance Integral Expr where
  toInteger (Integer i) = fromIntegral i
  toInteger (Float f) = truncate f
  quotRem (Integer i) (Integer j) = (Integer (i `quot` j), Integer (i `rem` j))
  (Integer i) `mod` (Integer j) = Integer (i `mod` j)
  (Integer i) `mod` (Float j) = Float (fromIntegral i `mod'` j)
  (Float i) `mod` (Integer j) = Float (i `mod'` fromIntegral j)
  (Float i) `mod` (Float j) = Float (i `mod'` j)

instance Show Expr where
  show (Integer i) = show i
  show (Float f) = show f
  show (Var v) = v
  show (Operator o) = o
  show (List l) = show l
  show (ArithmeticOp o e1 e2) = show e1 ++ " " ++ show o ++ " " ++ show e2
  show (Function n args e) = n ++ "(" ++ unwords args ++ ") = " ++ show e
  show (If c t e) = "if " ++ show c ++ " then " ++ show t ++ " else " ++ show e
  show (Callable n args) = n ++ "(" ++ unwords (map show args) ++ ")"
  show (Declarator n) = n
  show (Call n) = n

data AST a = Empty | Node a [AST a]

instance Eq a => Eq (AST a) where
  Empty == Empty = True
  (Node a xs) == (Node b ys) = a == b && xs == ys
  _ == _ = False

instance (Show a) => Show (AST a) where
  show Empty = ""
  show (Node a []) = show a
  show (Node a xs) = show a ++ " -> \n" ++ show xs

instance Functor AST where
  fmap f Empty = Empty
  fmap f (Node a xs) = Node (f a) (map (fmap f) xs)

data Env = Env { variables :: [(Expr, Expr)] } deriving (Show)

instance Eq Env where
    (Env xs) == (Env ys) = xs == ys

instance Semigroup Env where
  Env xs <> Env ys = Env (removeDuplicates (ys ++ xs))
    where
      removeDuplicates = nubBy (\(k1, _) (k2, _) -> k1 == k2)

instance Monoid Env where
  mempty = Env []

emptyEnv :: Env
emptyEnv = Env []
