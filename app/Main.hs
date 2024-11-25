module Main (main) where

import System.Environment
import Lib
import OptParser

main :: IO ()
main =  do
  options <- getOptions
  print options
  putStrLn "Hello, Haskell!"
  putStrLn "This is the main function of the app"
