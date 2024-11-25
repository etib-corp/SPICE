module Main where

import Test.HUnit
import OptParser
import Options.Applicative
import System.Environment

testOptionsParsing :: Test
testOptionsParsing = TestList
  [ TestCase $ do
      let args = ["--verbose", "--input", "input.txt", "--output", "output.txt"]
          result = execParserPure defaultPrefs (info (options <**> helper) fullDesc) args
      case result of
        Success opts -> do
          assertEqual "Verbose should be True" True (optVerbose opts)
          assertEqual "Input should be 'input.txt'" "input.txt" (optInput opts)
          assertEqual "Output should be 'output.txt'" "output.txt" (optOutput opts)
        _ -> assertFailure "Parsing failed when it should have succeeded"
  , TestCase $ do
      let args = ["--input", "input.txt", "--output", "output.txt"]
          result = execParserPure defaultPrefs (info (options <**> helper) fullDesc) args
      case result of
        Success opts -> do
          assertEqual "Verbose should be False" False (optVerbose opts)
          assertEqual "Input should be 'input.txt'" "input.txt" (optInput opts)
          assertEqual "Output should be 'output.txt'" "output.txt" (optOutput opts)
        _ -> assertFailure "Parsing failed when it should have succeeded"
  , TestCase $ do
      let args = ["--verbose", "--input", "input.txt"]
          result = execParserPure defaultPrefs (info (options <**> helper) fullDesc) args
      case result of
        Failure _ -> return () -- Expected failure
        _ -> assertFailure "Parsing succeeded when it should have failed"
  , TestCase $ do
      let args = ["--verbose", "--output", "output.txt"]
          result = execParserPure defaultPrefs (info (options <**> helper) fullDesc) args
      case result of
        Failure _ -> return () -- Expected failure
        _ -> assertFailure "Parsing succeeded when it should have failed"
  , TestCase $ do
      let args = ["--input", "input.txt", "--output", "output.txt"]
          result = execParserPure defaultPrefs (info (options <**> helper) fullDesc) args
      case result of
        Failure _ -> return () -- Expected failure
        _ -> assertFailure "Parsing succeeded when it should have failed"
  ]

main :: IO ()
main = do
  _ <- runTestTT testOptionsParsing
  return ()
