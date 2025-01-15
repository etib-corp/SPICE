module Main (main) where

import System.Environment
import Lib
import OptParser
import Prompt
import Files
import Control.Applicative
import System.Exit
import LispParser
import Structures
import MyParser
import Ast
import Eval

getFile :: [String] -> IO String
getFile ([x]) = secureGetContent x <|> error
  where
    error = exitWith (ExitFailure 84)

parseFileLine :: String -> Env -> IO Env
parseFileLine content e = case parse content parseExpression of
  Left err -> putStrLn ("Error: " ++ show err) >> return e
  Right expr -> do eval (createAst expr) e

parseFileWithStartEnv :: String -> Env -> IO Env
parseFileWithStartEnv content e = case lines content of
    [] -> return e
    (x:xs) -> do
      env <- parseFileLine x e
      -- print env : If you want to print the environment after each line
      parseFileWithStartEnv (unlines xs) env

parseFile :: String -> IO Env
parseFile content = parseFileWithStartEnv content emptyEnv

compiler :: String -> Env -> IO Env
compiler str env = case parse str parseExpression of
  Left err -> do
    putStrLn $ "Error: " ++ show err
    return env -- Return the current environment (or some default Env)
  Right expr -> eval (createAst expr) env

choseMode :: Options -> IO ()
choseMode (Options v f) = case length f of
  0 -> performCLI defaultPrompt emptyEnv compiler
  _ -> getFile f >>= parseFile >> return ()

main :: IO ()
main = choseMode =<< getOptions
