module Prompt
    (performCLI, defaultPrompt, returnString)
 where

import Prelude

import Lib

import Data.Char

import System.IO
import System.Exit

type Command = String
data CLI = Prompt { prompt :: String, history :: [Command], command :: Command } deriving(Show)

fillCLI :: Command -> CLI -> CLI
fillCLI cmd (Prompt p h _) = Prompt p (h ++ [cmd]) cmd

displayHelp :: IO ()
displayHelp = putStrLn "HELP"

interpreter :: Command -> (String -> String) -> IO ()
interpreter "quit" _ = exitSuccess
interpreter "help" _ = displayHelp
interpreter cmd perf = (putStrLn $ (perf cmd))

performCLI :: CLI -> (String -> String) -> IO ()
performCLI cli perform = getInput >>= (\cmd -> (compile cmd) >> (recurse cmd))
    where
        getInput    = (putStr (prompt cli) *> hFlush stdout *> getLine)
        compile cmd = interpreter cmd perform
        recurse cmd = (performCLI (fillCLI cmd cli) perform)

defaultPrompt :: CLI
defaultPrompt = Prompt "<SPICE> " [] ""

returnString :: String -> String
returnString str = str
