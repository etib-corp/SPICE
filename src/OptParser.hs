module OptParser where

import Options.Applicative
import System.Exit
import System.IO.Error;
import System.Environment


data Options = Options
  { optVerbose :: Bool
  , optConfigFile :: String
  , files :: [String]
  }

instance Show Options where
  show (Options a c b) = "Options { optVerbose = " ++ show a ++ ", optConfigFile = " ++ show c ++ ", files = " ++ show b ++ " }"

options :: Parser Options
options = Options
  <$> switch
    ( long "verbose"
    <> short 'v'
    <> help "Enable verbose messages" )
  <*> (strOption
    ( long "config"
    <> short 'c'
    <> metavar "FILE"
    <> help "Config file that contains your language syntax."))
  <*> many (argument str (metavar "FILES..."))


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
  (Options v1 c1 f1) == (Options v2 c2 f2) = v1 == v2 && f1 == f2 && c1 == c2
