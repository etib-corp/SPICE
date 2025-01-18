module Prompt
    (performCLI, defaultPrompt, returnString)
 where

import Prelude

import Lib

import Data.Char
import Structures

import System.IO
import System.Exit

-- | Type alias for the command which is basically a string.
type Command = String

-- | Data type for the CLI. Represents the prompt, history and the current command.
data CLI = Prompt { prompt :: String, history :: [Command], command :: Command } deriving(Show)

-- | Returns the string representation of the given environment.
fillCLI :: Command -> CLI -> CLI
fillCLI cmd (Prompt p h _) = Prompt p (h ++ [cmd]) cmd

-- | Returns the string representation of the given environment.
interpreter :: Command -> Env -> (String -> Env -> IO Env) -> IO Env
interpreter "quit" _ _ = exitSuccess
interpreter "env" e _ = putStrLn (show e) >> return e
interpreter cmd env perf = perf cmd env

-- | Perform the CLI using a given CLI, environment and perform function.
performCLI :: CLI -> Env -> (String -> Env -> IO Env) -> IO ()
performCLI cli env perform = getInput >>= (\cmd -> (compile cmd env) >>= \e  -> (recurse cmd e))
    where
        catchAndGet     = do
            isClosed <- isEOF
            if isClosed then exitSuccess else getLine
        getInput        = (putStr (prompt cli) *> hFlush stdout *> catchAndGet)
        compile cmd env = interpreter cmd env perform
        recurse cmd env = (performCLI (fillCLI cmd cli) env perform)

-- | Default prompt of SPICE project.
defaultPrompt :: CLI
defaultPrompt = Prompt "<SPICE> " [] ""
