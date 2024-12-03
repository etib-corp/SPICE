module Main where

import Test.HUnit
import OptParser
import Options.Applicative
import System.Environment

testOptionsParsing :: Test
testOptionsParsing = TestList
  [ TestCase $ do
      let args = ["--verbose", "file1.txt", "file2.txt"]
          result = execParserPure defaultPrefs (info (options <**> helper) fullDesc) args
      case result of
        Success opts -> do
          assertEqual "Verbose should be True" True (optVerbose opts)
          assertEqual "Files should match" ["file1.txt", "file2.txt"] (files opts)
        _ -> assertFailure "Parsing failed when it should have succeeded"
  , TestCase $ do
      let args = ["file1.txt", "file2.txt"]
          result = execParserPure defaultPrefs (info (options <**> helper) fullDesc) args
      case result of
        Success opts -> do
          assertEqual "Verbose should be False" False (optVerbose opts)
          assertEqual "Files should match" ["file1.txt", "file2.txt"] (files opts)
        _ -> assertFailure "Parsing failed when it should have succeeded"
  ]

testShowOptions :: Test
testShowOptions = TestList
  [ TestCase $ do
      let opts = Options { optVerbose = True, files = ["file1.txt", "file2.txt"] }
          expected = "Options { optVerbose = True, files = [\"file1.txt\",\"file2.txt\"] }"
      assertEqual "Show instance should match expected" expected (show opts)
  , TestCase $ do
      let opts = Options { optVerbose = False, files = ["file1.txt", "file2.txt"] }
          expected = "Options { optVerbose = False, files = [\"file1.txt\",\"file2.txt\"] }"
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
