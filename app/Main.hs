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
import Ast
import Eval
import Data.Word
import OpCode
import GLPInterface
import Debug.Trace
import Execute

getFile :: [String] -> IO String
getFile ([x]) = secureGetContent x <|> error
  where
    error = exitWith (ExitFailure 84)

parseFileLine :: Options -> String -> Programm -> ParserConfig -> IO Programm
parseFileLine o content e NullConfig = case parse content parseExpression of
  Left err -> putStrLn ("Error: " ++ show err) >> return e
  Right expr -> executeProgramm o (createAst expr) e
parseFileLine o content e parse = case parseWithConfig content parse of
  Left err -> putStrLn ("Error: " ++ show err) >> return e
  Right expr -> executeProgramm o (createAst expr) e

parseFileWithStartEnv :: Options -> String -> Programm -> ParserConfig -> IO Programm
parseFileWithStartEnv o content e parse = case lines content of
    [] -> return e
    (x:xs) -> do
      env <- parseFileLine o x e parse
      parseFileWithStartEnv o (unlines xs) env parse

parseFile :: Options -> String -> ParserConfig -> IO Programm
parseFile o content parse = parseFileWithStartEnv o content emptyProgramm parse

compiler :: String -> Programm -> Options -> ParserConfig -> IO Programm
compiler str p o NullConfig = case parse str parseExpression of
  Left err -> do
    putStrLn $ "Error: " ++ show err
    return p
  Right expr -> executeProgramm o (createAst expr) p
compiler str p o c = case parseWithConfig str c of
  Left err -> do
    putStrLn $ "Error: " ++ show err
    return p
  Right expr -> executeProgramm o (createAst expr) p

getConfigFile :: String -> IO ParserConfig
getConfigFile "" = pure NullConfig
getConfigFile x = loadParserConfiguration x

choseMode :: Options -> IO ()
choseMode (Options v cf f e c r) = case e of
  False -> case length f of
    0 -> getConfigFile cf >>= (\x -> performCLI defaultPrompt emptyProgramm compiler (Options v cf f e c r) x)
    _ -> getFile f >>= (\x -> getConfigFile cf >>= (\y -> parseFile (Options v cf f e c r) x y)) >> return ()
  True -> secureGetContent (head f) >>= executeVM (head f)

main :: IO ()
main = choseMode =<< getOptions
