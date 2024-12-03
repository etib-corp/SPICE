module OptParser where

import Options.Applicative
import System.Exit
import System.IO.Error;
import System.Environment


data Options = Options
  { optVerbose :: Bool
  , files :: [String]
  }

instance Show Options where
  show (Options a b) = "Options { optVerbose = " ++ show a ++ ", files = " ++ show b ++ " }"

options :: Parser Options
options = Option
  <$> switch
    ( long "verbose"
    <> short 'v'
    <> help "Enable verbose messages" )
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
