module Ast where

import Structures

createAst :: Expr -> AST Expr
createAst (Int i) = Node (Int i) []
createAst (Float f) = Node (Float f) []
createAst (Var v) = Node (Var v) []
createAst (Operator o) = Node (Operator o) []
createAst (ArithmeticOp op e1 e2) = Node (Operator op) [createAst e1, createAst e2]
createAst (Function s l e) = Node (Declarator s) [Node (List (map Var l)) [], createAst e]
createAst (If e1 c1 e2 r1) = Node (Condition "if") [createAst e1, Node (Condition c1) [], createAst e2, createAst r1]
createAst (Callable s l) = Node (Call s) [Node (List l) []]
createAst (List (x:xs)) = Node (List []) (map createAst (x:xs))

