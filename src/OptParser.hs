module OptParser where

import Options.Applicative
import System.Exit
import System.IO.Error;
import System.Environment


data Options = Options
  { optVerbose :: Bool
  , optInput :: String
  , optOutput :: String
  }

instance Show Options where
  show (Options v i o) = "Options { optVerbose = " ++ show v ++ ", optInput = " ++ i ++ ", optOutput = " ++ o ++ " }"

options :: Parser Options
options = Options
  <$> switch
    ( long "verbose"
    <> short 'v'
    <> help "Enable verbose messages" )
  <*> strOption
    ( long "input"
    <> short 'i'
    <> metavar "INPUT"
    <> help "Input file" )
  <*> strOption
    ( long "output"
    <> short 'o'
    <> metavar "OUTPUT"
    <> help "Output file" )

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
