module Structures where

data Atom = Integer Int | Str String | Operator String | Name String deriving (Show, Eq)
data Expression = Atom Atom | List [Expression] deriving (Show, Eq)

data AST = Empty | Node (AST) Expression (AST) deriving(Show, Eq)

-- constructAST :: AST -> Expression -> AST
-- constructAST tree (Atom a) = Node Empty (Atom a) Empty
-- constructAST tree (List lst) = 

createAST :: Expression -> AST
createAST (Atom val) = Node Empty (Atom val) Empty

