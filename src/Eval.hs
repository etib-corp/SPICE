module Eval where

import Structures
import Control.Monad (foldM)  -- Import foldM to handle monads
import System.Exit
import Control.Applicative
import Ast
import OpCode
import Debug.Trace
import Data.Word
import OptParser
import Lib
import Programm
import qualified Data.Map as Map
import DumpFile

import Debug.Trace (trace)

getInstruction :: AST Expr -> [BasicBlock] -> Either String [Instruction]
getInstruction (Node (Operator "=") ((Node (Var v) []):e2:[])) e =
  case (e2) of
    (Node (Integer i) []) -> Right $ [Set v (IntValue i)]
    _ -> case getInstruction e2 e of
      Right i -> Right $ i ++ [Set v (PopValue)]
      Left err -> Left err
getInstruction (Node (Operator "define") ((Node (Var v) []):e2:[])) e =
  case (e2) of
    (Node (Integer i) []) -> Right $ [Set v (IntValue i)]
    _ -> case getInstruction e2 e of
      Right i -> Right $ i ++ [Set v (PopValue)]
      Left err -> Left err
getInstruction (Node (Integer i) []) _ = Right $ [PushInt i]
-- getInstructions (Node (Float f) []) _ = Right $ PushFloat f
getInstruction (Node (Var v) []) e = Right $ [Get v]
getInstruction (Node (Operator "+") (e1:e2:[])) e =
  case (getInstruction e1 e, getInstruction e2 e) of
    (Right i1, Right i2) -> Right $ i1 ++ i2 ++ [Add]
    (Left err, _) -> Left err
    (_, Left err) -> Left err
getInstruction (Node (Operator "-") (e1:e2:[])) e =
  case (getInstruction e1 e, getInstruction e2 e) of
    (Right i1, Right i2) -> Right $ i1 ++ i2 ++ [Sub]
    (Left err, _) -> Left err
    (_, Left err) -> Left err
getInstruction (Node (Operator "*") (e1:e2:[])) e =
  case (getInstruction e1 e, getInstruction e2 e) of
    (Right i1, Right i2) -> Right $ i1 ++ i2 ++ [Mul]
    (Left err, _) -> Left err
    (_, Left err) -> Left err
getInstruction (Node (Statement "if") (c:t:e:[])) p =
  case (getInstruction c p, getInstruction t p, getInstruction e p) of
    (Right c', Right t', Right e') -> Right $ c' ++ [JumpIf ((length t') + 1)] ++ t' ++ [Jump (length e' + 1)]  ++ e'
    (Left err, _, _) -> Left err
    (_, Left err, _) -> Left err
    (_, _, Left err) -> Left err
getInstruction (Node (Operator "eq?") (e1:e2:[])) e =
  case (getInstruction e1 e, getInstruction e2 e) of
    (Right i1, Right i2) -> Right $ i1 ++ i2 ++ [Eq]
    (Left err, _) -> Left err
    (_, Left err) -> Left err
getInstruction (Node (Operator "<") (e1:e2:[])) e =
  case (getInstruction e1 e, getInstruction e2 e) of
    (Right i1, Right i2) -> Right $ i1 ++ i2 ++ [Less]
    (Left err, _) -> Left err
    (_, Left err) -> Left err
getInstruction (Node (Operator ">") (e1:e2:[])) e =
  case (getInstruction e1 e, getInstruction e2 e) of
    (Right i1, Right i2) -> Right $ i1 ++ i2 ++ [Greater]
    (Left err, _) -> Left err
    (_, Left err) -> Left err
getInstruction (Node (List  []) l) e = foldM (\acc x -> getInstruction x e >>= \newI -> return (acc ++ newI)) [] l
getInstruction (Node (Call "print") [Node (List argNodes) []]) e =
  case length argNodes of
    1 -> case createInstructionsFunction argNodes e of
      Right i -> Right $ head i ++ [Print]
      Left err -> Left err
    _ -> Left "print only accepts one argument."
getInstruction (Node (Call s) [Node (List argNodes) []]) e =
  case createInstructionsFunction argNodes e of
    Right i -> Right $ [CallFunc s i]
    Left err -> Left err

createInstructionsFunction :: [Expr] -> [BasicBlock] -> Either String [[Instruction]]
createInstructionsFunction [] _ = Right []
createInstructionsFunction (x:xs) e = case getInstruction (createAst x) e of
  Right i -> case createInstructionsFunction xs e of
    Right is -> Right $ i : is
    Left err -> Left err
  Left err -> Left err

walker2 :: AST Expr -> Programm -> Either String Programm
walker2 (Node (Var v) []) p = Right (p { instructionsP = instructionsP p ++ [Get v] })
walker2 (Node (Operator "=") ((Node (Var v) []):e2:[])) p =
  case (e2) of
    (Node (Integer i) []) -> Right (p { instructionsP = instructionsP p ++ [Set v (IntValue i)] })
    _ -> case getInstruction e2 (blocks p) of
      Right i -> Right $ p { instructionsP = instructionsP p ++ i ++ [Set v (PopValue)] }
      Left err -> Left err
walker2 (Node (Operator "define") ((Node (Var v) []):e2:[])) p =
  case (e2) of
    (Node (Integer i) []) -> Right (p { instructionsP = instructionsP p ++ [Set v (IntValue i)] })
    _ -> case getInstruction e2 (blocks p) of
      Right i -> Right $ p { instructionsP = instructionsP p ++ i ++ [Set v (PopValue)] }
      Left err -> Left err
walker2 (Node (Operator "+") (e1:e2:[])) p =
  case (getInstruction e1 (blocks p), getInstruction e2 (blocks p)) of
    (Right i1, Right i2) -> Right $ p { instructionsP = instructionsP p ++ i2 ++ i2 ++ [Add] }
    (Left err, _) -> Left err
    (_, Left err) -> Left err
walker2 (Node (Operator "-") (e1:e2:[])) p =
  case (getInstruction e1 (blocks p), getInstruction e2 (blocks p)) of
    (Right i1, Right i2) -> Right $ p { instructionsP = instructionsP p ++ i1 ++ i2 ++ [Sub] }
    (Left err, _) -> Left err
    (_, Left err) -> Left err
walker2 (Node (Operator "*") (e1:e2:[])) p =
  case (getInstruction e1 (blocks p), getInstruction e2 (blocks p)) of
    (Right i1, Right i2) -> Right $ p { instructionsP = instructionsP p ++ i1 ++ i2 ++ [Mul] }
    (Left err, _) -> Left err
    (_, Left err) -> Left err
walker2 (Node (Statement "if") (c:t:e:[])) p =
  case (getInstruction c (blocks p), getInstruction t (blocks p), getInstruction e (blocks p)) of
    (Right c', Right t', Right e') -> Right $ p { instructionsP = instructionsP p ++ c' ++ [JumpIf ((length t') + 1)] ++ t' ++ [Jump (length e' + 1)]  ++ e' }
    (Left err, _, _) -> Left err
    (_, Left err, _) -> Left err
    (_, _, Left err) -> Left err
walker2 (Node (Operator "eq?") (e1:e2:[])) p =
  case (getInstruction e1 (blocks p), getInstruction e2 (blocks p)) of
    (Right i1, Right i2) -> Right $ p { instructionsP = instructionsP p ++ i1 ++ i2 ++ [Eq] }
    (Left err, _) -> Left err
    (_, Left err) -> Left err
walker2 (Node (Operator "<") (e1:e2:[])) p =
  case (getInstruction e1 (blocks p), getInstruction e2 (blocks p)) of
    (Right i1, Right i2) -> Right $ p { instructionsP = instructionsP p ++ i1 ++ i2 ++ [Less] }
    (Left err, _) -> Left err
    (_, Left err) -> Left err
walker2 (Node (Operator ">") (e1:e2:[])) p =
  case (getInstruction e1 (blocks p), getInstruction e2 (blocks p)) of
    (Right i1, Right i2) -> Right $ p { instructionsP = instructionsP p ++ i1 ++ i2 ++ [Greater] }
    (Left err, _) -> Left err
    (_, Left err) -> Left err
walker2 (Node (List  []) l) p = foldM (\acc x -> walker2 x acc >>= \newP -> return newP) p l
walker2 (Node (Declarator s) (var:exp:[])) p =
  case getBasicBlocks s (blocks p) of
    Just bb -> Left ("*** ERROR : " ++ s ++ " already exists.")
    Nothing -> case getInstruction exp (blocks p) of
      Right i -> case var of
        (Node (List vars) []) -> Right $ p { blocks = blocks p ++ [BasicBlock s i (fmap (\(Var v) -> v) vars)] }
        _ -> Left ("*** ERROR : " ++ show var ++ " is not a list.") >> return p
      Left err -> Left err
walker2 (Node (Call "print") [Node (List argNodes) []]) p =
  case length argNodes of
    1 -> case createInstructionsFunction argNodes (blocks p) of
      Right i -> Right $ p { instructionsP = instructionsP p ++ head i ++ [Print] }
      Left err -> Left err
    _ -> Left "print only accepts one argument."
walker2 (Node (Call s) [Node (List argNodes) []]) p =
  case createInstructionsFunction argNodes (blocks p) of
    Right i -> Right $ p { instructionsP = instructionsP p ++ [CallFunc s i] }
    Left err -> Left err

execute :: [Instruction] -> VM -> VM
execute [] vm = vm
execute (op:ops) vm =
  case op of
    Set name (IntValue n) ->
      execute ops vm { envBackend = Map.insert name n (envBackend vm) }
    PushInt n ->
      execute ops vm { stack = n : stack vm }
    Set name PopValue ->
      case stack vm of
        (x:xs) -> execute ops vm { stack = xs, envBackend = Map.insert name x (envBackend vm) }
        _ -> error "Stack underflow on Set"
    Get name ->
      case Map.lookup name (envBackend vm) of
        Just value -> execute ops vm { stack = value : stack vm }
        Nothing -> error $ "Variable not found: " ++ name
    Add ->
      case stack vm of
        (x:y:xs) -> execute ops vm { stack = (y + x) : xs }
        _ -> error "Stack underflow on Add"
    Sub ->
      case stack vm of
        (x:y:xs) -> execute ops vm { stack = (y - x) : xs }
        _ -> error "Stack underflow on Sub"
    Print ->
      case stack vm of
        (x:xs) -> trace (show x) $ execute ops vm { stack = xs }
        _ -> error "Stack underflow on Print"
    PrintStack ->
        trace ("Stack -> " ++ show (stack vm)) $ execute ops vm
    Jump n ->
      let remainingOps = drop n (op:ops)
       in execute remainingOps vm
    JumpIf n ->
      case stack vm of
        (x:xs) ->
          if x == 0
            then let remainingOps = drop (n + 1) (op:ops)
                  in execute remainingOps vm { stack = xs }
            else execute ops vm { stack = xs }
        [] -> error "Stack underflow on JumpIf"


eval :: Options -> Programm -> IO Programm
eval (Options _ _ _ _ True r) (Programm b e i) = case instructionsToOpCode2 i b of
    Left c -> printRepresenation c >> writeProgramm c (Programm b e i) >> return (Programm b e i)
    Right err -> putStrLn err >> return (Programm b e [])
    where printRepresenation c = case r of
            True -> print (opCodesToInstructions c) >> print c
            False -> return ()
eval (Options _ _ _ _ False r) p = case programmEval p [] of
  Right (p, s) -> case popStack s of
    Just (ValueInt i, s') -> putStrLn (show i) >> return p
    _ -> return p
  Left m -> print m >> return p

executeProgramm :: Options -> AST Expr -> Programm -> IO Programm
executeProgramm o ast p = case walker2 ast p of
  Right p -> writeFile "out.bin" "" >> eval o p
  Left err -> putStrLn err >> return p
