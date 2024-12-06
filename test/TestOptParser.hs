module TestOptParser where
import Test.HUnit

import OptParser
import Options.Applicative
import System.Exit (ExitCode(..))
import System.Exit (exitFailure)
import System.IO.Error (IOError)

-- Test pour la fonction `options`
testOptionsVerboseFlag :: Test
testOptionsVerboseFlag = TestCase $ do
  let args = ["--verbose"]
  let expected = Options { optVerbose = True, files = [] }
  let result = parseArgs args
  assertEqual "Verbose flag should be parsed correctly" expected result

testOptionsFiles :: Test
testOptionsFiles = TestCase $ do
  let args = ["file1.txt", "file2.txt"]
  let expected = Options { optVerbose = False, files = ["file1.txt", "file2.txt"] }
  let result = parseArgs args
  assertEqual "Files should be parsed correctly" expected result

testOptionsVerboseAndFiles :: Test
testOptionsVerboseAndFiles = TestCase $ do
  let args = ["--verbose", "file1.txt", "file2.txt"]
  let expected = Options { optVerbose = True, files = ["file1.txt", "file2.txt"] }
  let result = parseArgs args
  assertEqual "Verbose and files should be parsed correctly" expected result

-- Fonction d'aide pour simuler l'analyse des arguments
parseArgs :: [String] -> Options
parseArgs args = case execParserPure customPrefs opts args of
  Success opts -> opts
  _            -> error "Failed to parse options"
  where
    opts = info (options <**> helper) fullDesc
    customPrefs = prefs showHelpOnError

-- -- Test pour la fonction `handleParseResultCustom`
-- testHandleParseResultSuccess :: Test
-- testHandleParseResultSuccess = TestCase $ do
--   let opts = Options { optVerbose = True, files = ["file1.txt"] }
--   result <- handleParseResultCustom (Success opts)
--   assertEqual "handleParseResultCustom should return options correctly" opts result

-- testHandleParseResultFailure :: Test
-- testHandleParseResultFailure = TestCase $ do
--   -- Créer un ParserFailure avec une fonction qui renvoie un message d'erreur et un exit code
--   let failure = Failure (ParserFailure (\_ -> ("Test failure", ExitFailure 1, 1)) [])
--   result <- handleParseResultCustom failure
--   -- L'erreur sera gérée et la fonction doit effectuer une action
--   -- selon le code de sortie (ici ExitFailure 1)
--   return ()



parseOptions :: [String] -> Either String Options
parseOptions args = case execParserPure defaultPrefs optsParser args of
    Success opts -> Right opts
    Failure err  -> Left (show err)  -- Show the error as a string
  where
    optsParser = info
      (options <**> helper)
      (fullDesc
        <> progDesc "A program that processes files with optional verbose output"
        <> header "File Processor - an example program")



-- Test pour vérifier que l'option --verbose fonctionne
testVerboseOption :: Test
testVerboseOption = TestCase $ do
  let result = parseOptions ["--verbose", "file1.txt", "file2.txt"]
  case result of
    Right (Options True _) -> return ()
    _ -> assertFailure "Verbose option was not correctly parsed"

-- Test pour vérifier que l'option -v fonctionne (version courte de verbose)
testShortVerboseOption :: Test
testShortVerboseOption = TestCase $ do
  let result = parseOptions ["-v", "file1.txt"]
  case result of
    Right (Options True _) -> return ()
    _ -> assertFailure "Short verbose option was not correctly parsed"

-- Test pour vérifier la gestion des fichiers sans option verbose
testFilesOption :: Test
testFilesOption = TestCase $ do
  let result = parseOptions ["file1.txt", "file2.txt"]
  case result of
    Right (Options False ["file1.txt", "file2.txt"]) -> return ()
    _ -> assertFailure "Files were not parsed correctly"

-- Test pour vérifier qu'aucun fichier ne produit une liste vide
testNoFilesOption :: Test
testNoFilesOption = TestCase $ do
  let result = parseOptions []
  case result of
    Right (Options False []) -> return ()
    _ -> assertFailure "No files should result in an empty list"


-- Regroupement des tests
testOptParser :: Test
testOptParser = TestList
  [
    testOptionsVerboseFlag
  , testOptionsFiles
  , testOptionsVerboseAndFiles

  , testVerboseOption
  , testShortVerboseOption
  , testFilesOption
  , testNoFilesOption

--   , testHandleParseResultSuccess
--   , testHandleParseResultFailure
  ]
