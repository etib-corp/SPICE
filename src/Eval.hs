module Eval where

import Structures
import Control.Monad (foldM)  -- Import foldM to handle monads
import System.Exit
import Control.Applicative
import Ast
import Debug.Trace

pushEnv :: Expr -> Expr -> Env -> Env
pushEnv k v e = e <> Env [(k, v)]

getInEnv :: Expr -> Env -> Maybe Expr
getInEnv k (Env []) = Nothing
getInEnv k (Env ((k', v):xs)) | k == k' = Just v
                              | otherwise = getInEnv k (Env xs)

baseOp :: Expr -> Expr -> (Expr -> Expr -> Expr) -> Env -> Either String Expr
baseOp e1 e2 op env = case (e1, e2) of
  (Integer i1, Integer i2) -> Right (op e1 e2)
  (Float f1, Float f2) -> Right (op e1 e2)
  (Integer i, Float f) -> Right (op e1 e2)
  (Float f, Integer i) -> Right (op e1 e2)
  (Var v, Integer i) -> case getInEnv (Var v) env of
    Just (Integer i') -> Right (op (Integer i') e2)
    Just (Float f') -> Right (op (Float f') e2)
    _ -> Left ("Variable " ++ show v ++ " is not bound.")
  (Integer i, Var v) -> case getInEnv (Var v) env of
    Just (Integer i') -> Right (op e1 (Integer i'))
    Just (Float f') -> Right (op e1 (Float f'))
    _ -> Left ("Variable " ++ show v ++ " is not bound.")
  (Var v1, Var v2) -> case (getInEnv (Var v1) env, getInEnv (Var v2) env) of
    (Just e1', Just e2') -> baseOp e1' e2' op env
    _ -> Left ("Variables " ++ show v1 ++ " or " ++ show v2 ++ " is not bound.")
  (Var v, Float f) -> case getInEnv (Var v) env of
    Just (Integer i') -> Right (op (Integer i') e2)
    Just (Float f') -> Right (op (Float f') e2)
    _ -> Left ("Variable " ++ show v ++ " is not bound.")
  (Float f, Var v) -> case getInEnv (Var v) env of
    Just (Integer i') -> Right (op e1 (Integer i'))
    Just (Float f') -> Right (op e1 (Float f'))
    _ -> Left ("Variable " ++ show v ++ " is not bound.")
  _ -> Left "Invalid operation: incompatible types"

checkEquality :: AST Expr -> AST Expr -> Env -> Either String Bool
checkEquality e1 e2 env = case (getExpr e1 env, getExpr e2 env) of
  (Right e1', Right e2') ->
    case (e1', e2') of
      (Var v1, Var v2) -> case (getInEnv (Var v1) env, getInEnv (Var v2) env) of
        (Just e1'', Just e2'') -> Right $ e1'' == e2''
        _ -> Left ("*** ERROR : variables " ++ show v1 ++ " or " ++ show v2 ++ " are not bound.")
      (Var v, e) -> case getInEnv (Var v) env of
        Just e' -> Right $  e' == e
        _ -> Left ("*** ERROR : variable " ++ show v ++ " is not bound.")
      (e, Var v) -> case getInEnv (Var v) env of
        Just e' -> Right $ e' == e
        _ -> Left ("*** ERROR : variable " ++ show v ++ " is not bound.")
      _ -> Right (e1' == e2')
  (Left err, _) -> Left err
  (_, Left err) -> Left err
  _ -> Left "Invalid operation: incompatible types"

getExpr :: AST Expr -> Env -> Either String Expr
getExpr (Node (Integer i) []) _ = Right (Integer i)
getExpr (Node (Float f) []) _ = Right (Float f)
getExpr (Node (Var v) []) e = Right (Var v)
getExpr (Node (Operator "+") (e1:e2:[])) e =
  case (getExpr e1 e, getExpr e2 e) of
    (Right e1', Right e2') -> baseOp e1' e2' (+) e
    (Left err, _) -> Left err
    (_, Left err) -> Left err
getExpr (Node (Operator "-") (e1:e2:[])) e =
  case (getExpr e1 e, getExpr e2 e) of
    (Right e1', Right e2') -> baseOp e1' e2' (-) e
    (Left err, _) -> Left err
    (_, Left err) -> Left err
getExpr (Node (Operator "*") (e1:e2:[])) e =
  case (getExpr e1 e, getExpr e2 e) of
    (Right e1', Right e2') -> baseOp e1' e2' (*) e
    (Left err, _) -> Left err
    (_, Left err) -> Left err
getExpr (Node (Operator "div") (e1:e2:[])) e =
  case (getExpr e1 e, getExpr e2 e) of
    (Right e1', Right e2') -> baseOp e1' e2' (/) e
    (Left err, _) -> Left err
    (_, Left err) -> Left err
getExpr (Node (Operator "mod") (e1:e2:[])) e =
  case (getExpr e1 e, getExpr e2 e) of
    (Right e1', Right e2') -> baseOp e1' e2' mod e
    (Left err, _) -> Left err
    (_, Left err) -> Left err
getExpr (Node (Declarator s) []) e = Right $ Declarator s
getExpr (Node (List l) []) e = Right $ List (l)
getExpr (Node (Call s) [Node (List argNodes) []]) e = case getInEnv (Declarator s) e of
  Just (List [List vars, exp]) ->
    if length argNodes == length vars
      then case (createStack argNodes vars e) of
        Right env -> getExpr (createAst exp) env
        Left err -> Left err
      else Left ("*** ERROR : function " ++ s ++ " expects " ++ show (length vars) ++
                  " arguments, but " ++ show (length argNodes) ++ " were given.")
  Nothing -> Left ("*** ERROR : function " ++ s ++ " is not bound.")
getExpr (Node (Operator "eq?") (e1:e2:[])) e = case checkEquality e1 e2 e of
  Right True -> Right (Integer 1)
  Right False -> Right (Integer 0)
  Left err -> Left err
getExpr (Node (Statement "if") (c:t:e:[])) env =
  case getExpr c env of
    Right (Var v) -> case getInEnv (Var v) env of
      Just (Integer i) | i == 0 -> getExpr e env
                       | otherwise -> getExpr t env
      Just (Float f) | f == 0 -> getExpr e env
                     | otherwise -> getExpr t env
      _ -> Left ("*** ERROR : variable " ++ v ++ " is not bound.")
    Right (Integer i) | i == 0 -> getExpr e env
                      | otherwise -> getExpr t env
    Right (Float f) | f == 0 -> getExpr e env
                    | otherwise -> getExpr t env
    _ -> Left ("*** ERROR : invalid condition.")
getExpr s e = trace (show s) (error "Invalid expression")

getOnlyExpr :: AST Expr -> Expr
getOnlyExpr (Node (Integer i) []) = Integer i
getOnlyExpr (Node (Float f) []) = Float f
getOnlyExpr (Node (Var v) []) = Var v
getOnlyExpr (Node (Operator "+") (e1:e2:[])) = ArithmeticOp ("+") (getOnlyExpr e1) (getOnlyExpr e2)
getOnlyExpr (Node (Operator "-") (e1:e2:[])) = ArithmeticOp ("-") (getOnlyExpr e1) (getOnlyExpr e2)
getOnlyExpr (Node (Operator "*") (e1:e2:[])) = ArithmeticOp ("*") (getOnlyExpr e1) (getOnlyExpr e2)
getOnlyExpr (Node (Operator "div") (e1:e2:[])) = ArithmeticOp ("div") (getOnlyExpr e1) (getOnlyExpr e2)
getOnlyExpr (Node (Operator "mod") (e1:e2:[])) = ArithmeticOp ("mod") (getOnlyExpr e1) (getOnlyExpr e2)
getOnlyExpr (Node (Operator "eq?") (e1:e2:[])) = ArithmeticOp ("eq?") (getOnlyExpr e1) (getOnlyExpr e2)
getOnlyExpr (Node (Statement "if") (c:t:e:[])) = If (getOnlyExpr c) (getOnlyExpr t) (getOnlyExpr e)
getOnlyExpr (Node (Call s) [Node (List argNodes) []]) = Callable s (fmap getOnlyExpr (fmap createAst argNodes))
getOnlyExpr (Node (Declarator s) []) = Declarator s
getOnlyExpr (Node (List l) []) = List l

createStack :: [Expr] -> [Expr] -> Env -> Either String Env
createStack [] [] env = Right env
createStack (x:xs) (y:ys) env = case getExpr (createAst x) env of
  Right x' -> createStack xs ys (pushEnv y x' env)
  Left err -> Left err

exucuteArithmetic :: AST Expr -> AST Expr -> (Expr -> Expr -> Expr) -> Env -> IO Env
exucuteArithmetic e1 e2 op e =
  case (getExpr e1 e, getExpr e2 e) of
  (Right e1', Right e2') -> case baseOp e1' e2' op e of
    Right e' -> print e' >> return e
    Left err -> putStrLn ("*** ERROR: " ++ err) >> return e
  (Left err, _) -> putStrLn ("*** ERROR: " ++ err) >> return e
  (_, Left err) -> putStrLn ("*** ERROR: " ++ err) >> return e

walker :: AST Expr -> Env -> IO Env
walker (Node (Integer i) []) e = print i >> return e
walker (Node (Float f) []) e = print f >> return e
walker (Node (Var v) []) e = do
  let v' = getInEnv (Var v) e
  case v' of
    Just v'' -> print v'' >> return e
    Nothing -> putStrLn ("*** ERROR : variable " ++ v ++" is not bound.") >> return e
walker (Node (Operator "=") (e1:e2:[])) env =
  case (getExpr e1 env, getExpr e2 env) of
    (Right e1', Right e2') -> return . pushEnv e1' e2' $ env
    (Left err, _) -> putStrLn err >> return env
    (_, Left err) -> putStrLn err >> return env
walker (Node (Operator "define") (e1:e2:[])) env =
  case (getExpr e1 env, getExpr e2 env) of
    (Right e1', Right e2') -> return . pushEnv e1' e2' $ env
    (Left err, _) -> putStrLn err >> return env
    (_, Left err) -> putStrLn err >> return env
walker (Node (Operator "+") (e1:e2:[])) env = exucuteArithmetic e1 e2 (+) env
walker (Node (Operator "-") (e1:e2:[])) env = exucuteArithmetic e1 e2 (-) env
walker (Node (Operator "*") (e1:e2:[])) env = exucuteArithmetic e1 e2 (*) env
walker (Node (Operator "div") (e1:e2:[])) env = exucuteArithmetic e1 e2 (/) env
walker (Node (Operator "/") (e1:e2:[])) env = exucuteArithmetic e1 e2 (/) env
walker (Node (Operator "mod") (e1:e2:[])) env = exucuteArithmetic e1 e2 mod env
walker (Node (Operator "eq?") (e1:e2:[])) env = case checkEquality e1 e2 env of
  Right True -> putStrLn "#t" >> return env
  Right False -> putStrLn "#f" >> return env
  Left err -> putStrLn err >> return env
walker (Node (Operator "<") (e1:e2:[])) env = case (getExpr e1) env < (getExpr e2) env of
  True -> putStrLn "#t" >> return env
  False -> putStrLn "#f" >> return env
walker (Node (List []) l) e = foldM (\acc x -> walker x acc >>= \newEnv -> return (acc <> newEnv)) e l
walker (Node (Declarator s) (var:exp:[])) e =
  case getExpr var e of
    Right (List vars) -> return $ pushEnv (Declarator s) (List [List vars, getOnlyExpr exp]) e
    Right _ -> putStrLn ("*** ERROR : " ++ show var ++ " is not a list.") >> return e
    Left err -> putStrLn err >> return e
walker (Node (Call s) [Node (List argNodes) []]) env =
  case getInEnv (Declarator s) env of
    Just (List [List vars, exp]) -> case length argNodes == length vars of
      True -> case createStack argNodes vars env of
        Right env' -> walker (createAst exp) (env' <> (Env [(Declarator s, List [List vars, exp])])) >> return env
        Left err -> putStrLn err >> return env
      False -> putStrLn ("*** ERROR : function " ++ s ++ " expects " ++ show (length vars) ++
                         " arguments, but " ++ show (length argNodes) ++ " were given.") >> return env
    Nothing -> putStrLn ("*** ERROR : function " ++ s ++ " is not bound.") >> return env
  where
    execute :: Expr -> Env -> IO Env
    execute e env = walker (createAst e) env
walker (Node (Statement "if") (c:t:e:[])) env =
  case getExpr c env of
    Right (Var v) -> case getInEnv (Var v) env of
      Just (Integer i) | i == 0 -> walker t env
                       | otherwise -> walker e env
      Just (Float f) | f == 0 -> walker t env
                     | otherwise -> walker e env
      _ -> putStrLn ("*** ERROR : variable " ++ v ++ " is not bound.") >> return env
    Right (Integer i) | i == 0 -> walker e env
                      | otherwise -> walker t env
    Right (Float f) | f == 0 -> walker e env
                    | otherwise -> walker t env
    _ -> putStrLn ("*** ERROR : invalid condition.") >> return env
walker _ e = return e

eval :: AST Expr -> Env -> IO Env
eval ast env = walker ast env
