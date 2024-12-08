module TestLib where
import Test.HUnit
import Lib
import System.IO
import System.IO.Temp (withSystemTempFile)
import System.IO.Silently (capture)
import System.IO.Silently (capture_)
import System.IO (hPutStrLn, hClose)


-- Test de la fonction getInt
testGetInt :: Test
testGetInt = TestCase $ do
    assertEqual "Should parse integer correctly" (Just 42) (getInt "42")
    assertEqual "Should return Nothing for non-integer string" Nothing (getInt "abc")
    assertEqual "Should return Nothing for string with extra characters" Nothing (getInt "42abc")

-- Test de la fonction getDouble
testGetDouble :: Test
testGetDouble = TestCase $ do
    assertEqual "Should parse double correctly" (Just 3.14) (getDouble "3.14")
    assertEqual "Should return Nothing for non-numeric string" Nothing (getDouble "abc")
    assertEqual "Should return Nothing for string with extra characters" Nothing (getDouble "3.14abc")

-- Test de la fonction isWhiteSpace
testIsWhiteSpace :: Test
testIsWhiteSpace = TestCase $ do
    assertEqual "Should return True for space character" True (isWhiteSpace ' ')
    assertEqual "Should return True for newline character" True (isWhiteSpace '\n')
    assertEqual "Should return True for tab character" True (isWhiteSpace '\t')
    assertEqual "Should return False for non-whitespace character" False (isWhiteSpace 'a')

-- Test de la fonction wLast
testWLast :: Test
testWLast = TestCase $ do
    assertEqual "Should return empty string for empty input" "" (wLast "")
    assertEqual "Should return empty string for single character input" "" (wLast "a")
    assertEqual "Should remove the last character" "hel" (wLast "hello")

-- Test de la fonction lastChar
testLastChar :: Test
testLastChar = TestCase $ do
    assertEqual "Should return '\0' for empty string" '\0' (lastChar "")
    assertEqual "Should return single character for single character string" 'a' (lastChar "a")
    assertEqual "Should return last character for multi-character string" 'o' (lastChar "hello")

-- Test de la fonction catFile
testCatFile :: Test
testCatFile = TestCase $ do
    -- Crée un fichier temporaire pour tester catFile
    withSystemTempFile "test.txt" $ \path handle -> do
        -- Ecrire du contenu dans le fichier temporaire
        hPutStrLn handle "Hello, World!"
        hClose handle
        -- Utiliser `catFile` pour afficher le contenu du fichier
        output <- captureOutput (catFile path)
        assertEqual "Should print the content of the file" "Hello, World!\n" output

-- Capture la sortie de la fonction IO
captureOutput :: IO () -> IO String
captureOutput action = do
    -- capture_ capture la sortie d'un IO action et retourne seulement la chaîne de caractères
    output <- capture_ action
    return output

-- Regroupement des tests
testLib :: Test
testLib = TestList
  [
    testGetInt
  , testGetDouble
  , testIsWhiteSpace
  , testWLast
  , testLastChar
  , testCatFile
  ]
