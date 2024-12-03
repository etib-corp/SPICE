module Prompt
    (performCLI, defaultPrompt, returnString)
 where

import Prelude

import Lib

import Data.Char
import Structures

import System.IO
import System.Exit

type Command = String
data CLI = Prompt { prompt :: String, history :: [Command], command :: Command } deriving(Show)

fillCLI :: Command -> CLI -> CLI
fillCLI cmd (Prompt p h _) = Prompt p (h ++ [cmd]) cmd

displayHelp :: IO ()
displayHelp = putStrLn "HELP"

interpreter :: Command -> Env -> (String -> Env -> IO ()) -> IO ()
interpreter "quit" _ _ = exitSuccess
interpreter "help" _ _ = displayHelp
interpreter cmd env perf = perf cmd env

performCLI :: CLI -> Env -> (String -> Env -> IO ()) -> IO ()
performCLI cli env perform = getInput >>= (\cmd -> (compile cmd env) >> (recurse cmd env))
    where
        getInput    = (putStr (prompt cli) *> hFlush stdout *> getLine)
        compile cmd env = interpreter cmd env perform
        recurse cmd env = (performCLI (fillCLI cmd cli) env perform)

defaultPrompt :: CLI
defaultPrompt = Prompt "<SPICE> " [] ""

returnString :: String -> String
returnString str = str
