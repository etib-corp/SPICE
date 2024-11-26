module Eval where

import Structures
import Control.Monad (foldM)  -- Import foldM to handle monads
import System.Exit
import Control.Applicative

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

walker :: AST Expr -> Env -> IO Env
walker (Node (Integer i) []) e = return e
walker (Node (Float f) []) e = return e
walker (Node (Var v) []) e = do
  let v' = getInEnv (Var v) e
  case v' of
    Just v'' -> print v'' >> return e
    Nothing -> putStrLn ("*** ERROR : variable " ++ v ++" is not bound.") >> return e
walker (Node (Operator "=") (e1:e2:[])) env = return . pushEnv (getExpr e1) (getExpr e2) $ env
walker (Node (List []) l) e = foldM (\acc x -> walker x acc >>= \newEnv -> return (acc <> newEnv)) e l

eval :: AST Expr -> IO Env
eval ast = walker ast (Env [])
