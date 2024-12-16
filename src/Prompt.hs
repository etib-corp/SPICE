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

interpreter :: Command -> Env -> (String -> Env -> IO Env) -> IO Env
interpreter "quit" _ _ = exitSuccess
interpreter "env" e _ = putStrLn (show e) >> return e
interpreter cmd env perf = perf cmd env

performCLI :: CLI -> Env -> (String -> Env -> IO Env) -> IO ()
performCLI cli env perform = getInput >>= (\cmd -> (compile cmd env) >>= \e  -> (recurse cmd e))
    where
        catchAndGet     = do
            isClosed <- isEOF
            if isClosed then exitSuccess else getLine
        getInput        = (putStr (prompt cli) *> hFlush stdout *> catchAndGet)
        compile cmd env = interpreter cmd env perform
        recurse cmd env = (performCLI (fillCLI cmd cli) env perform)

defaultPrompt :: CLI
defaultPrompt = Prompt "<SPICE> " [] ""

returnString :: String -> String
returnString str = str
