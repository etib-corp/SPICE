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
getFile (x:[]) = secureGetContent x <|> error
  where
    error = exitWith (ExitFailure 84)

parseFile :: String -> IO ()
parseFile content = case parse content parseExpression of
  Left err -> putStrLn $ "Error: " ++ show err
  Right expr -> eval (createAst expr) emptyEnv >> return ()

compiler :: String -> Env -> IO Env
compiler str env = case parse str parseExpression of
  Left err -> do
    putStrLn $ "Error: " ++ show err
    return env -- Return the current environment (or some default Env)
  Right expr -> eval (createAst expr) env

choseMode :: Options -> IO ()
choseMode (Options v f) = case length f of
  0 -> performCLI defaultPrompt emptyEnv compiler
  _ -> getFile f >>= parseFile

main :: IO ()
main = choseMode =<< getOptions
