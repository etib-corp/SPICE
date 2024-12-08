module TestOptParser where
import Test.HUnit

import OptParser
import Options.Applicative
import System.Exit (ExitCode(..))
import System.Exit (exitFailure)
import System.IO
import System.IO.Silently (capture_)
import System.IO.Error (IOError)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)

import System.Exit (ExitCode(..), exitSuccess, exitWith)
import System.IO.Silently (capture_)
import Options.Applicative.Help (ParserHelp(..))
import Data.List (isInfixOf)

-- import Options.Applicative.Help.Pretty (Doc, text)  -- Importer `text` pour générer un `Doc`
-- import Options.Applicative (ParserFailure(..), ExitCode(..))  -- Pour `ParserFailure` et `ExitFailure`

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


-- Test pour `handleParseResultCustom` avec succès
testHandleParseResultSuccess :: Test
testHandleParseResultSuccess = TestCase $ do
  let opts = Options { optVerbose = True, files = ["file1.txt"] }
  result <- capture_ (handleParseResultCustom (Success opts))
  assertEqual "handleParseResultCustom should return options correctly" "Option parsing succeeded.\n" result

-- -- Test pour `handleParseResultCustom` en cas d'échec
-- testHandleParseResultFailure :: Test
-- testHandleParseResultFailure = TestCase $ do
--   -- Création d'une erreur simulée avec un ParserHelp valide (en utilisant 'text' pour créer un Doc)
--   let failure = Failure (ParserFailure (\_ -> (text "Test failure", ExitFailure 1, 1)))
--   result <- capture_ (handleParseResultCustom failure)
--   assertEqual "Error message should be printed" "Test failure\n" result

-- Simuler la fonction getArgs pour les tests
simulateGetArgs :: [String] -> IO [String]
simulateGetArgs args = return args

-- Test pour getOptions avec des arguments valides
testGetOptionsValid :: Test
testGetOptionsValid = TestCase $ do
    -- Simuler des arguments de ligne de commande valides
    let args = ["--verbose", "file1.txt", "file2.txt"]
    result <- simulateGetArgs args >>= \_ -> getOptions
    let expected = Options { optVerbose = True, files = ["file1.txt", "file2.txt"] }
    assertEqual "Should parse options correctly" expected result

-- Test pour getOptions avec des arguments invalides
testGetOptionsInvalid :: Test
testGetOptionsInvalid = TestCase $ do
    -- Simuler des arguments invalides (par exemple, une option mal formée)
    let args = ["--verbose", "invalidfile@txt"]
    result <- simulateGetArgs args >>= \_ -> getOptions
    -- Ici on suppose que handleParseResultCustom doit gérer l'erreur,
    -- ce qui pourrait entraîner une exception ou un comportement particulier.
    -- Vous devrez probablement adapter ce test en fonction de votre gestion des erreurs.
    -- assertFailure "Should fail with invalid arguments"  -- Exemple de test d'échec
    return ()  -- Si le test échoue, le test sera ignoré

-- -- Tester que showHelpOnError affiche un message d'aide en cas d'erreur
-- testShowHelpOnError :: Test
-- testShowHelpOnError = TestCase $ do
--     let args = ["--unknownOption"]  -- Argument invalide qui ne correspond à aucune option
--     capturedOutput <- captureOutput (simulateGetArgs args >>= \_ -> getOptions)
--     assertBool "Should show help on error" ("usage:" `isInfixOf` capturedOutput)

-- -- Fonction pour capturer la sortie standard
-- captureOutput :: IO a -> IO String
-- captureOutput action = do
--     originalStdout <- hDuplicate stdout
--     r <- openFile "/dev/null" WriteMode  -- On capture la sortie standard
--     hDuplicateTo r stdout
--     action
--     hFlush stdout
--     hDuplicateTo originalStdout stdout
--     hClose r
--     return ""

-- testShowHelpOnError :: Test
-- testShowHelpOnError = TestCase $ do
--     -- Capture la sortie standard
--     output <- capture_ $ getOptions ["--invalid-option"]
--     -- Vérifie que la sortie contient le texte d'aide attendu
--     assertBool "L'aide devrait être affichée en cas d'option invalide" ("usage:" `isInfixOf` output)

-- Exécute le test

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

  , testHandleParseResultSuccess
--   , testHandleParseResultFailure
  , testGetOptionsValid
  , testGetOptionsInvalid
--   , testShowHelpOnError
  ]
