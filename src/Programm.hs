module Programm where

import Structures
import Debug.Trace
import Lib
import OpCode

data Stack = ValueInstruction Instruction | ValueInt Int deriving (Show, Eq)

getIntStack :: Stack -> Maybe Int
getIntStack (ValueInt i) = Just i
getIntStack _ = Nothing

popStack :: [Stack] -> Maybe (Stack, [Stack])
popStack [] = Nothing
popStack (x:xs) = Just (x, xs)

pushEnv :: Env String Int -> Name -> Int -> Env String Int
pushEnv e n v = Env [(n, v)] <> e

arithmeticOpEval :: (Int -> Int -> Int) -> [Stack] -> Maybe [Stack]
arithmeticOpEval f s = case popStack s of
  Just (ValueInt i1, s') -> case popStack s' of
    Just (ValueInt i2, s'') -> Just (ValueInt (f i2 i1):s'')
    _ -> Nothing
  _ -> Nothing

createEnvFunction :: Env String Int -> [String] -> [[Instruction]] -> [BasicBlock] -> Maybe (Env String Int)
createEnvFunction e [] [] _ =  Just e
createEnvFunction e (x:xs) (i:is) b = case programmEval (Programm b e i) [] of
  Right (p, s) -> case popStack s of
    Just (ValueInt i, s') -> createEnvFunction (pushEnv e x i) xs is b
    _ -> Nothing
  _ -> Nothing


programmEval :: Programm -> [Stack] -> Either String (Programm, [Stack])
programmEval (Programm b e []) s =  Right (Programm b e [], s)
programmEval (Programm b e (PushInt i:xs)) s = programmEval (Programm b e xs) (ValueInt i:s)
programmEval (Programm b e (Set v (IntValue i):xs)) s = programmEval (Programm b (pushEnv e v i) xs) s
programmEval (Programm b e (Set v (PopValue):xs)) s = case popStack s of
  Just (ValueInt i, s') -> programmEval (Programm b (pushEnv e v i) xs) s'
  _ -> Left "Error"
programmEval (Programm b e (Get v:xs)) s = case lookup v (variables e) of
  Just i -> programmEval (Programm b e xs) (ValueInt i:s)
  _ -> Left "Variable not found"
programmEval (Programm b e (Add:xs)) s = case arithmeticOpEval (+) s of
  Just s' -> programmEval (Programm b e xs) s'
  _ -> Left "Invalid operation +"
programmEval (Programm b e (Sub:xs)) s = case arithmeticOpEval (-) s of
    Just s' -> programmEval (Programm b e xs) s'
    _ -> Left "Invalid operation -"
programmEval (Programm b e (Mul:xs)) s = case arithmeticOpEval (*) s of
    Just s' -> programmEval (Programm b e xs) s'
    _ -> Left "Invalid operation *"
programmEval (Programm b e (Div:xs)) s = case arithmeticOpEval div s of
    Just s' -> programmEval (Programm b e xs) s'
    _ -> Left "Invalid operation /"
programmEval (Programm b e (Mod:xs)) s = case arithmeticOpEval mod s of
    Just s' -> programmEval (Programm b e xs) s'
    _ -> Left "Invalid operation %"
programmEval (Programm b e (Less:xs)) s = case arithmeticOpEval (\ x y -> fromEnum (x < y)) s of
    Just s' -> programmEval (Programm b e xs) s'
    _ -> Left "Invalid operation <"
programmEval (Programm b e (LessEq:xs)) s = case arithmeticOpEval (\ x y -> fromEnum (x <= y)) s of
    Just s' -> programmEval (Programm b e xs) s'
    _ -> Left "Invalid operation <="
programmEval (Programm b e (Eq:xs)) s = case arithmeticOpEval (\ x y -> fromEnum (x == y)) s of
    Just s' -> programmEval (Programm b e xs) s'
    _ -> Left "Invalid operation =="
programmEval (Programm b e (JumpIf j:xs)) s = case popStack s of
  Just (ValueInt i, s') -> case i of
    0 -> programmEval (Programm b e (drop j xs)) s'
    _ -> programmEval (Programm b e xs) s'
  _ -> Left "Invalid operation JumpIf"
programmEval (Programm b e (Jump i:xs)) s = programmEval (Programm b e (drop i xs)) s
programmEval (Programm b e (Print:xs)) s = case popStack s of
  Just (ValueInt i, s') -> trace (show i) $ programmEval (Programm b e xs) s'
  _ -> Left "Error"
programmEval (Programm b e (CallFunc n i:xs)) s = case getBasicBlocks n b of
  Just (BasicBlock _ e' p) -> case length i == length p of
    True -> case createEnvFunction e p i b of
        Just e'' -> case programmEval (Programm b e'' e') s of
          Left m -> Left m
          Right (p, s') -> programmEval (Programm b e xs) s'
        _ -> Left "Error"
    False -> Left "Invalid number of arguments"
  _ -> Left "Function not found"
programmEval p s = trace (show p) $ Left "Error"
