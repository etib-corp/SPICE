module Eval where

import Structures

getExpr :: AST Expr -> Expr
getExpr (Node (Integer i) []) = Integer i
getExpr (Node (Float f) []) = Float f
getExpr (Node (Var v) []) = Var v

pushEnv :: Expr -> Expr -> Env -> Env
pushEnv k v e = e <> Env [(k, v)]

getInEnv :: Expr -> Env -> Maybe Expr
getInEnv k (Env []) = Nothing
getInEnv k (Env ((k', v):xs)) | k == k' = Just v
                              | otherwise = getInEnv k (Env xs)

walker :: AST Expr -> Env -> Env
walker Empty e = e
walker (Node (Var a) []) e = case getInEnv (Var a) e of
  Just v -> e
  Nothing -> e
walker (Node (Integer a) []) e = e
walker (Node (Operator "=") (e1:e2:[])) e = pushEnv (getExpr e1) (getExpr e2) e
walker (Node (List []) l) e = foldl (\acc x -> acc <> walker x acc) e l

