module Prompt
    (performCLI, defaultPrompt)
 where

import Prelude

import Lib

import Data.Char
import Structures
import OptParser

import System.IO
import System.Exit

-- | Type alias for the command which is basically a string.
type Command = String

-- | Data type for the CLI. Represents the prompt, history and the current command.
data CLI = Prompt { prompt :: String, history :: [Command], command :: Command } deriving(Show)

-- | Returns the string representation of the given environment.
fillCLI :: Command -> CLI -> CLI
fillCLI cmd (Prompt p h _) = Prompt p (h ++ [cmd]) cmd

displayHelp :: IO ()
displayHelp = putStrLn "HELP"

interpreter :: Command -> Programm -> (String -> Programm -> Options -> ParserConfig -> IO Programm) -> Options -> ParserConfig -> IO Programm
interpreter "quit" _ _ _ _ = exitSuccess
interpreter "env" e _ _ _ = putStrLn (show e) >> return e
interpreter cmd env perf o p = perf cmd env o p

performCLI :: CLI -> Programm -> (String -> Programm -> Options -> ParserConfig -> IO Programm) -> Options -> ParserConfig -> IO ()
performCLI cli p perform o c = getInput >>= (\cmd -> (compile cmd p o c) >>= \e  -> (recurse cmd e o c))
    where
        catchAndGet     = do
            isClosed <- isEOF
            if isClosed then exitSuccess else getLine
        getInput        = (putStr (prompt cli) *> hFlush stdout *> catchAndGet)
        compile cmd env o c = interpreter cmd env perform o c
        recurse cmd env o c = (performCLI (fillCLI cmd cli) env perform o c)

-- | Default prompt of SPICE project.
defaultPrompt :: CLI
defaultPrompt = Prompt "<SPICE> " [] ""
