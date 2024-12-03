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

getFile :: [String] -> IO String
getFile (x:[]) = secureGetContent x <|> error
  where
    error = exitWith (ExitFailure 84)

parseFile :: String -> IO ()
parseFile content = case parse content parseLispExpressionTest of
  Left err -> putStrLn $ "Error: " ++ show err
  Right expr -> putStrLn $ show expr

choseMode :: Options -> IO ()
choseMode (Options v f) = case length f of
  0 -> performCLI defaultPrompt returnString
  _ -> getFile f >>= parseFile


main :: IO ()
main = choseMode =<< getOptions
