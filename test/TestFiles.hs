module TestFiles where

import Test.HUnit
import System.IO.Error (catchIOError, IOError)
import Files (secureGetContent)
import System.IO.Temp (withSystemTempFile)
import Control.Exception (try, SomeException)
import System.IO (hPutStrLn, hClose)

-- Test pour vérifier que secureGetContent fonctionne correctement pour un fichier existant
testSecureGetContentSuccess :: Test
testSecureGetContentSuccess = TestCase $ do
    -- Crée un fichier temporaire pour tester
    withSystemTempFile "test.txt" $ \path handle -> do
        let content = "Hello, World!"
        -- Écrire du contenu dans le fichier
        hPutStrLn handle content
        hClose handle
        -- Test de secureGetContent
        result <- secureGetContent path
        assertEqual "Content should match the written content" content result

-- Test pour vérifier que secureGetContent gère l'erreur lorsqu'un fichier n'existe pas
testSecureGetContentFailure :: Test
testSecureGetContentFailure = TestCase $ do
    -- Essayer de lire un fichier qui n'existe pas
    result <- try (secureGetContent "nonexistent.txt") :: IO (Either SomeException String)
    case result of
        Left _ -> return ()  -- Si une exception est lancée, c'est attendu
        Right _ -> assertFailure "An error should have been thrown for a non-existent file"

-- Regroupement des tests
testFiles :: Test
testFiles = TestList
  [ testSecureGetContentSuccess
  , testSecureGetContentFailure
  ]
