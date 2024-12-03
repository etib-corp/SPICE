module Main (main) where

import System.Environment
import Lib
import OptParser
import Prompt
import LispParser
import Structures

compiler :: String -> Env -> IO ()
compiler str env = parse str parseLispExpressionTest


choseMode :: Options -> IO ()
choseMode (Options v f) = case length f of
  0 -> performCLI defaultPrompt emptyEnv compiler
  _ -> putStrLn "One mode selected"

main :: IO ()
main = choseMode =<< getOptions
