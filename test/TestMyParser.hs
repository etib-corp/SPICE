{-# LANGUAGE ScopedTypeVariables #-}
module TestMyParser where

import Test.HUnit
import MyParser
import Test.HUnit hiding (test)
-- import Test.HUnit hiding (test)  -- Cacher la fonction 'test' de HUnit pour éviter l'ambiguïté
import Control.Exception (evaluate)
import Control.Exception (evaluate, SomeException, try)

-- Test pour la fonction parseName
testParseName :: Test
testParseName = TestCase $ do
    let result1 = parse "John123" parseName
    let result2 = parse "123John" parseName
    let result3 = parse "John123!" parseName
    assertEqual "Should parse a valid name" (Right "John123") result1
    assertEqual "Should fail to parse invalid name starting with digit" (Left (Error "Char doesn't exist... but how ?" 0)) result2
    assertEqual "Should fail to parse name with invalid character" (Left (Error "Char doesn't exist... but how ?" 0)) result3

-- Test pour la fonction parseInt
testParseInt :: Test
testParseInt = TestCase $ do
    let result1 = parse "-123" parseInt
    let result2 = parse "456" parseInt
    let result3 = parse "abc" parseInt
    let result4 = parse "3." parseInt   -- Entrée invalide (point flottant)
    assertEqual "Should parse a valid signed integer" (Right (-123)) result1
    assertEqual "Should parse a valid unsigned integer" (Right 456) result2
    assertEqual "Should fail for invalid input" (Left (Error "Invalid signed number" 0)) result3
    assertEqual "Should fail for '3.'" (Left (Error "Invalid signed number" 0)) result4

-- Test pour la fonction parseUInt
testParseUInt :: Test
testParseUInt = TestCase $ do
    let result1 = parse "123" parseUInt
    let result2 = parse "abc" parseUInt
    assertEqual "Should parse a valid unsigned integer" (Right 123) result1
    assertEqual "Should fail for non-numeric input" (Left (Error "Invalid unsigned number" 0)) result2

-- Test pour la fonction parseGivenString
testParseGivenString :: Test
testParseGivenString = TestCase $ do
    let result1 = parse "hello" (parseGivenString "hello")
    let result2 = parse "helloworld" (parseGivenString "hello")
    let result3 = parse "world" (parseGivenString "hello")
    assertEqual "Should parse the given string correctly" (Right "hello") result1
    assertEqual "Should fail if string doesn't match exactly" (Left (Error "Char doesn't exist... but how ?" 0)) result2
    assertEqual "Should fail for incorrect string match" (Left (Error "Char doesn't exist... but how ?" 0)) result3

-- Test pour la gestion des erreurs dans parseName
testParseNameError :: Test
testParseNameError = TestCase $ do
    let result = parse "123" parseName
    assertEqual "Should fail for names starting with a number" (Left (Error "Char doesn't exist... but how ?" 0)) result

-- Test pour la fonction parseNoneOf
testParseNoneOf :: Test
testParseNoneOf = TestCase $ do
    -- Test pour un caractère qui n'est pas dans la chaîne donnée
    let result1 = parse "d" (parseNoneOf "abc")  -- 'd' n'est pas dans "abc"
    let result2 = parse "b" (parseNoneOf "abc")  -- 'b' est dans "abc", devrait échouer
    let result3 = parse "z" (parseNoneOf "xyz")  -- 'z' n'est pas dans "xyz"

    -- Vérifier que le test de réussite fonctionne
    assertEqual "Should parse 'd' as it is not in 'abc'" (Right 'd') result1
    -- Vérifier que le test d'échec fonctionne
    assertEqual "Should fail when 'b' is in 'abc'" (Left (Error "Char doesn't exist... but how ?" 0)) result2
    -- Vérifier que 'z' n'est pas dans 'xyz' et donc doit réussir
    assertEqual "Should parse 'z' as it is not in 'xyz'" (Right 'z') result3

-- Test pour la fonction parseString
testParseString :: Test
testParseString = TestCase $ do
    -- Test avec une chaîne qui contient un espace
    let result1 = parse "hello world" parseString
    let result2 = parse "hello\tworld" parseString  -- Test avec un tab
    let result3 = parse "hello\nworld" parseString  -- Test avec une nouvelle ligne

    -- Test avec une chaîne sans espace blanc
    let result4 = parse "helloworld" parseString

    -- Vérifier que la chaîne est bien lue jusqu'à l'espace blanc
    assertEqual "Should parse 'hello' until space" (Right "hello") result1
    assertEqual "Should parse 'hello' until tab" (Right "hello") result2
    assertEqual "Should parse 'hello' until newline" (Right "hello") result3
    -- Vérifier que la fonction échoue lorsque la chaîne n'a pas d'espace blanc
    assertEqual "Should parse the whole string without whitespace" (Right "helloworld") result4


-- Test pour la fonction parseSepBy
testParseSepBy :: Test
testParseSepBy = TestCase $ do
    -- Test avec un séparateur (ici, un espace)
    let result1 = parse "1 2 3" (parseSepBy parseInt (parseChar ' '))
    let result2 = parse "1" (parseSepBy parseInt (parseChar ' '))  -- Un seul élément, sans séparateur après
    let result3 = parse "" (parseSepBy parseInt (parseChar ' '))  -- Aucun élément, chaîne vide

    -- Vérifier que la fonction renvoie la bonne liste d'entiers
    assertEqual "Should parse '1 2 3' into [1,2,3]" (Right [1, 2, 3]) result1
    -- Vérifier que la fonction renvoie un seul élément pour "1"
    assertEqual "Should parse '1' into [1]" (Right [1]) result2
    -- Vérifier que la fonction renvoie une liste vide pour une chaîne vide
    assertEqual "Should return an empty list for empty input" (Right []) result3

-- Test pour la fonction parseWhiteSpaces
testParseWhiteSpaces :: Test
testParseWhiteSpaces = TestCase $ do
    -- Test avec des espaces
    let result1 = parse "   hello" parseWhiteSpaces
    -- Test avec un tab
    let result2 = parse "\thello" parseWhiteSpaces
    -- Test avec un retour à la ligne
    let result3 = parse "\nhello" parseWhiteSpaces
    -- Test avec une chaîne sans espaces
    let result4 = parse "hello" parseWhiteSpaces

    -- Vérifier que la fonction consomme correctement les espaces blancs
    assertEqual "Should parse spaces correctly" (Right ()) result1
    assertEqual "Should parse tab correctly" (Right ()) result2
    assertEqual "Should parse newline correctly" (Right ()) result3
    assertEqual "Should return nothing if no spaces are present" (Left (Error "Char doesn't exist... but how ?" 0)) result4

-- Test pour la fonction parseDouble
testParseDouble :: Test
testParseDouble = TestCase $ do
    -- Test avec un nombre flottant valide
    let result1 = parse "3.14" parseDouble
    let result2 = parse "-3.14" parseDouble
    let result3 = parse "0.99" parseDouble

    -- Test avec un nombre invalide
    let result4 = parse "3." parseDouble
    let result5 = parse ".99" parseDouble
    let result6 = parse "abc" parseDouble

    -- Vérifier les résultats valides
    assertEqual "Should parse '3.14' correctly" (Right 3.14) result1
    assertEqual "Should parse '-3.14' correctly" (Right (-3.14)) result2
    assertEqual "Should parse '0.99' correctly" (Right 0.99) result3

    -- Vérifier les résultats invalides
    assertEqual "Should fail for '3.'" (Left (Error "Invalid float" 0)) result4
    assertEqual "Should fail for '.99'" (Left (Error "Invalid float" 0)) result5
    assertEqual "Should fail for 'abc'" (Left (Error "Invalid float" 0)) result6

-- -- Test pour la fonction test
-- testTestFunction :: Test
-- testTestFunction = TestCase $ do
--     -- Test avec un Right, la fonction doit retourner l'expression
--     let result1 = test (Right "valid expression")  -- Exemple avec une chaîne ou une expression valide
--     assertEqual "Should return the expression from Right" "valid expression" result1

--     -- Test avec un Left, la fonction doit lancer une exception
--     let result2 = evaluate (test (Left (Error "Some error" 0)))  -- Tester avec un Left pour voir l'exception
--     assertException "Should throw an exception for Left" result2

-- Fonction d'assistance pour tester les exceptions
assertException :: String -> IO a -> IO ()
assertException msg action = do
    result <- try action
    case result of
        Left (_ :: SomeException) -> return ()  -- L'exception a été lancée, ce qui est attendu
        Right _ -> assertFailure msg  -- Si aucune exception n'est lancée, échoue le test


-- Regroupement de tous les tests
testMyParser :: Test
testMyParser = TestList
    [ testParseName
    , testParseInt
    , testParseUInt
    , testParseGivenString
    , testParseNameError
    , testParseNoneOf
    , testParseString
    , testParseSepBy
    , testParseWhiteSpaces
    , testParseDouble
    -- , testTestFunction
    ]
