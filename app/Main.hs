module Main (main) where

import Lib
import Prompt

main :: IO ()
main = performCLI defaultPrompt returnString
