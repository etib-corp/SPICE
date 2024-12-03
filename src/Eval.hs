module Eval where

import Structures
import Control.Monad (foldM)  -- Import foldM to handle monads
import System.Exit
import Control.Applicative
import Ast

pushEnv :: Expr -> Expr -> Env -> Env
pushEnv k v e = e <> Env [(k, v)]

getInEnv :: Expr -> Env -> Maybe Expr
getInEnv k (Env []) = Nothing
getInEnv k (Env ((k', v):xs)) | k == k' = Just v
                              | otherwise = getInEnv k (Env xs)

baseOp :: Expr -> Expr -> (Expr -> Expr -> Expr) -> Env -> Maybe Expr
baseOp e1 e2 op env = case (e1, e2) of
  (Integer i1, Integer i2) -> Just (op e1 e2)
  (Float f1, Float f2) -> Just (op e1 e2)
  (Integer i, Float f) -> Just (op e1 e2)
  (Float f, Integer i) -> Just (op e1 e2)
  (Var v, Integer i) -> case getInEnv (Var v) env of
    Just (Integer i') -> Just (op (Integer i') e2)
    Just (Float f') -> Just (op (Float f') e2)
    _ -> Nothing
  (Integer i, Var v) -> case getInEnv (Var v) env of
    Just (Integer i') -> Just (op e1 (Integer i'))
    Just (Float f') -> Just (op e1 (Float f'))
    _ -> Nothing
  (Var v1, Var v2) -> case (getInEnv (Var v1) env, getInEnv (Var v2) env) of
    (Just e1', Just e2') -> baseOp e1' e2' op env
    (_, _) -> Nothing
  (Var v, Float f) -> case getInEnv (Var v) env of
    Just (Integer i') -> Just (op (Integer i') e2)
    Just (Float f') -> Just (op (Float f') e2)
    _ -> Nothing
  (Float f, Var v) -> case getInEnv (Var v) env of
    Just (Integer i') -> Just (op e1 (Integer i'))
    Just (Float f') -> Just (op e1 (Float f'))
    _ -> Nothing
  _ -> Nothing

getExpr :: AST Expr -> Env -> Expr
getExpr (Node (Integer i) []) _ = Integer i
getExpr (Node (Float f) []) _ = Float f
getExpr (Node (Var v) []) _ = Var v
getExpr (Node (Operator "+") (e1:e2:[])) e = case baseOp (getExpr e1 e) (getExpr e2 e) (+) e of
  Just e' -> e'
  Nothing -> ArithmeticOp "+" (getExpr e1 e) (getExpr e2 e)
getExpr (Node (Operator "-") (e1:e2:[])) e = case baseOp (getExpr e1 e) (getExpr e2 e) (-) e of
  Just e' -> e'
  Nothing -> ArithmeticOp "-" (getExpr e1 e) (getExpr e2 e)
getExpr (Node (Operator "*") (e1:e2:[])) e = case baseOp (getExpr e1 e) (getExpr e2 e) (*) e of
  Just e' -> e'
  Nothing -> ArithmeticOp "*" (getExpr e1 e) (getExpr e2 e)
getExpr (Node (Operator "div") (e1:e2:[])) e = case baseOp (getExpr e1 e) (getExpr e2 e) (/) e of
  Just e' -> e'
  Nothing -> ArithmeticOp "div" (getExpr e1 e) (getExpr e2 e)
getExpr (Node (Operator "mod") (e1:e2:[])) e = case baseOp (getExpr e1 e) (getExpr e2 e) mod e of
  Just e' -> e'
  Nothing -> ArithmeticOp "mod" (getExpr e1 e) (getExpr e2 e)
getExpr (Node (Declarator s) []) e = Declarator s
getExpr (Node (List l) []) e = List (l)

createStack :: [Expr] -> [Expr] -> Env -> Env
createStack [] [] env = env
createStack (x:xs) (y:ys) env = createStack xs ys (pushEnv y (getExpr (createAst x) env) env)

walker :: AST Expr -> Env -> IO Env
walker (Node (Integer i) []) e = return e
walker (Node (Float f) []) e = return e
walker (Node (Var v) []) e = do
  let v' = getInEnv (Var v) e
  case v' of
    Just v'' -> print v'' >> return e
    Nothing -> putStrLn ("*** ERROR : variable " ++ v ++" is not bound.") >> return e
walker (Node (Operator "=") (e1:e2:[])) env = return . pushEnv (getExpr e1 env) (getExpr e2 env) $ env
walker (Node (Operator "+") (e1:e2:[])) env = print (baseOp (getExpr e1 env) (getExpr e2 env) (+) env) >> return env
walker (Node (Operator "div") (e1:e2:[])) env = print (baseOp (getExpr e1 env) (getExpr e2 env) (/) env) >> return env
walker (Node (Operator "-") (e1:e2:[])) env = print (baseOp (getExpr e1 env) (getExpr e2 env) (-) env) >> return env
walker (Node (Operator "mod") (e1:e2:[])) env = print (baseOp (getExpr e1 env) (getExpr e2 env) mod env) >> return env
walker (Node (Operator "*") (e1:e2:[])) env = print (baseOp (getExpr e1 env) (getExpr e2 env) (*) env) >> return env
walker (Node (List []) l) e = foldM (\acc x -> walker x acc >>= \newEnv -> return (acc <> newEnv)) e l
walker (Node (Declarator s) (var:exp:[])) e = return . pushEnv (Declarator s) (List [getExpr var e, getExpr exp e]) $ e
walker (Node (Call s) [Node (List argNodes) []]) env =
  case getInEnv (Declarator s) env of
    Just (List [List vars, exp]) -> case length argNodes == length vars of
      True -> execute exp (createStack argNodes vars env) >> return env
      False -> putStrLn ("*** ERROR : function " ++ s ++ " expects " ++ show (length vars) ++
                         " arguments, but " ++ show (length argNodes) ++ " were given.") >> return env
    Nothing -> putStrLn ("*** ERROR : function " ++ s ++ " is not bound.") >> return env
  where
    -- Correct spelling and define `execute`
    execute :: Expr -> Env -> IO Env
    execute e env = walker (createAst e) env
walker _ e = return e

eval :: AST Expr -> IO Env
eval ast = walker ast (Env [])
