module Structures where

import MyParser

import Control.Monad
import Control.Applicative
import Control.Monad

import Data.List (nubBy)
import Data.Fixed (mod')
import qualified Data.Map as Map

-- | Type alias for the name of a variable or a function.
type Name = String

-- | Type alias for the formatter of an expression.
type Formatter = (String, String)

-- | Data type for the expression.
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
  deriving (Eq, Ord, Show)

-- | Data type for the parser configuration. It contains the different parsers for the different types of expressions.
data ParserConfig = ParserConfig {
    parseBoolean' :: Parser Expr
  , parseVariable :: (Formatter,[String])
  , parserOperator :: [(Formatter, [String], String)]
  , parseCondition :: Formatter
  , parseParameter :: Parser [String]
  , codeBlockConfig :: (Formatter, [String])
  , parseIf :: (Formatter,[String],[String])
  , parseFunction :: (Formatter, [String], [String])
  , parseCallable :: (Formatter, [String], String, [String])
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

-- instance Show Expr where
--      show (Integer i) = show i
--      show (Float f) = show f
--      show (Var v) = v
--      show (Operator o) = o
--      show (List l) = show l
--      show (ArithmeticOp o e1 e2) = show e1 ++ " " ++ show o ++ " " ++ show e2
--      show (Function n args e) = n ++ "(" ++ unwords args ++ ") = " ++ show e
--      show (If c t e) = "if " ++ show c ++ " then " ++ show t ++ " else " ++ show e
--      show (Callable n args) = n ++ "(" ++ unwords (map show args) ++ ")"
--      show (Declarator n) = n
--      show (Call n) = n

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

data Value = IntValue Int | PopValue deriving (Show, Eq)

data Instruction
  = PushInt Int
  | Get Name
  | Eq
  | NEq
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Less
  | LessEq
  | Greater
  | GreaterEq
  | Print
  | PrintStack
  | Jump Int
  | JumpIf Int
  | JumpIfNot Int
  | Pop
  | PopN Int
  | None
  | Return
  | CallFunc Name [[Instruction]]
  | Set Name Value
  deriving (Show, Eq)

data BasicBlock
 = BasicBlock { name :: Name
              , instructions :: [Instruction]
              , params :: [Name]
              } deriving (Show)

data Env a b = Env { variables :: [(a, b)] } deriving (Show, Eq)


instance Eq a => Semigroup (Env a b) where
  Env xs <> Env ys = Env (nubBy (\(a, _) (b, _) -> a == b) (xs ++ ys))

instance Eq a => Monoid (Env a b) where
  mempty = Env []

emptyEnv :: Env String Int
emptyEnv = Env []

emptyProgramm :: Programm
emptyProgramm = Programm [] emptyEnv []

data Programm = Programm { blocks :: [BasicBlock], envP :: Env String Int  , instructionsP :: [Instruction] } deriving (Show)

instance Semigroup Programm where
  Programm xs env1 is1 <> Programm ys env2 is2 = Programm (xs ++ ys) (env1 <> env2) (is1 ++ is2)


type StackBackend = [Int]
type EnvBackend = Map.Map String Int

data VM = VM
  { stack :: StackBackend,
    envBackend :: EnvBackend
  } deriving (Show)

emptyBasicBlock :: BasicBlock
emptyBasicBlock = BasicBlock "" [] []

emptyVm :: VM
emptyVm = VM { stack = [], envBackend = Map.empty }
