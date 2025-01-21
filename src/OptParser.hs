module OptParser where

import Options.Applicative
import System.Exit
import System.IO.Error
import System.Environment


data Options = Options
  { optVerbose :: Bool
  , optConfigFile :: String
  , files :: [String]
  , optExecute :: Bool
  , optCompile :: Bool
  , optPreprocess :: Bool
  }

instance Show Options where
  show (Options a f b e c p) = "Options { optVerbose = " ++ show a ++ ", files = " ++ show b ++ ", optConfigFile = " ++ show f ++ ", optExecute = " ++ show e ++ ", optCompile = " ++ show c ++ ", optPreprocess = " ++ show p ++ " }"

options :: Parser Options
options = Options <$> switch
  ( long "verbose"
  <> short 'v'
  <> help "Verbose output" )
  <*> strOption
  ( long "config"
  <> short 'c'
  <> value ""
  <> metavar "FILE"
  <> help "Configuration file" )
  <*> many (argument str (metavar "FILES..."))
  <*> switch
  ( long "execute"
  <> short 'e'
  <> help "Execute the program" )
  <*> switch
  ( long "compile"
  <> short 'C'
  <> help "Compile the program" )
  <*> switch
  ( long "preprocess"
  <> short 'P'
  <> help "Preprocess the program" )


handleParseResultCustom :: ParserResult Options -> IO Options
handleParseResultCustom result = case result of
  Success opts -> pure opts
  Failure failure ->
    let (msg, exitcode) = renderFailure failure "optparse-app"
    in putStrLn msg >> case exitcode of
      ExitSuccess -> exitSuccess
      ExitFailure n -> exitWith (ExitFailure n)

getOptions :: IO Options
getOptions = execParserPure customPrefs opts <$> getArgs >>= handleParseResultCustom
  where
    opts = info (options <**> helper)
      ( fullDesc )
    customPrefs = prefs $ showHelpOnError

instance Eq Options where
  (Options v1 f1 e1 c1 p1 fc1) == (Options v2 f2 e2 c2 p2 fc2) = v1 == v2 && f1 == f2 && e1 == e2 && c1 == c2 && p1 == p2 && fc1 == fc2
