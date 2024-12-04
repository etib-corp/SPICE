module Ast where

import Structures

createAst :: Expr -> AST Expr
createAst (Integer i) = Node (Integer i) []
createAst (Float f) = Node (Float f) []
createAst (Var v) = Node (Var v) []
createAst (Operator o) = Node (Operator o) []
createAst (ArithmeticOp op e1 e2) = Node (Operator op) [createAst e1, createAst e2]
createAst (Function s l e) = Node (Declarator s) [Node (List (map Var l)) [], createAst e]
createAst (If c e1 e2) = Node (Statement "if") [createAst c, createAst e1, createAst e2]
createAst (Callable s l) = Node (Call s) [Node (List l) []]
createAst (List l) = Node (List []) (fmap createAst l)
