module Main (main) where

import System.Environment
import Lib
import OptParser
import Prompt

choseMode :: Options -> IO ()
choseMode (Options v f) = case length f of
  0 -> performCLI defaultPrompt returnString
  _ -> putStrLn "One mode selected"

main :: IO ()
main = choseMode =<< getOptions
