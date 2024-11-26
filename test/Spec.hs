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

testShowOptions :: Test
testShowOptions = TestList
  [ TestCase $ do
      let opts = Options { optVerbose = True, optInput = "input.txt", optOutput = "output.txt" }
          expected = "Options {optVerbose = True, optInput = \"input.txt\", optOutput = \"output.txt\"}"
      assertEqual "Show instance should match expected" expected (show opts)
  , TestCase $ do
      let opts = Options { optVerbose = False, optInput = "input.txt", optOutput = "output.txt" }
          expected = "Options {optVerbose = False, optInput = \"input.txt\", optOutput = \"output.txt\"}"
      assertEqual "Show instance should match expected" expected (show opts)
  ]

testHelpOutput :: Test
testHelpOutput = TestList
  [ TestCase $ do
      let args = ["--help"]
          result = execParserPure defaultPrefs (info (options <**> helper) fullDesc) args
      case result of
        Success _ -> assertFailure "Parsing succeeded when it should have failed"
        _ -> return () -- Expected failure
  ]

main :: IO ()
main = do
  _ <- runTestTT $ TestList [testOptionsParsing, testShowOptions, testHelpOutput]
  return ()
