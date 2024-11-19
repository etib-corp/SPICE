module Ast where

import Structures

createAst :: Expr -> AST Expr
createAst e = case e of
  Int i -> Node Empty e Empty
  Float f -> Node Empty e Empty
  Var s -> Node Empty e Empty
  ArithmeticOp s e1 e2 -> Node (createAst e1) (Operator s) (createAst e2)
  Function s l e -> Node (Node Empty (List (map Var l)) Empty) (Callable s) (createAst e)
