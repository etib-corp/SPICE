module TestNewParser where

import Test.HUnit
import NewParser
import NewParser (parseExpo, parseNumber)
import NewParser (spaces, parseWhitespace)
import Control.Exception (evaluate)
import Data.Char (isDigit)

-- Test pour la fonction parseAny
testParseAny :: Test
testParseAny = TestCase $ do
    let result1 = runParser parseAny "a"  -- Test avec un caractère valide
    let result2 = runParser parseAny "123"  -- Test avec une chaîne de chiffres
    assertEqual "Should parse the first character" (Just ('a', "")) result1
    assertEqual "Should parse the first character of '123'" (Just ('1', "23")) result2

-- Test pour la fonction parseChar
testParseChar :: Test
testParseChar = TestCase $ do
    let result1 = runParser (parseChar 'a') "a"  -- Test avec le bon caractère
    let result2 = runParser (parseChar 'a') "b"  -- Test avec un caractère incorrect
    assertEqual "Should parse 'a' correctly" (Just ('a', "")) result1
    assertEqual "Should fail when 'b' is parsed" Nothing result2

-- Test pour la fonction parseAnyChar
testParseAnyChar :: Test
testParseAnyChar = TestCase $ do
    let result1 = runParser (parseAnyChar "abc") "a"  -- Test avec un caractère présent
    let result2 = runParser (parseAnyChar "abc") "d"  -- Test avec un caractère absent
    assertEqual "Should parse 'a' as it is in 'abc'" (Just ('a', "")) result1
    assertEqual "Should fail for 'd' as it is not in 'abc'" Nothing result2

-- Test pour la fonction parseManyUntil
testParseManyUntil :: Test
testParseManyUntil = TestCase $ do
    let result1 = runParser (parseManyUntil (parseChar 'a') (parseChar 'b')) "aab"
    let result2 = runParser (parseManyUntil (parseChar 'a') (parseChar 'b')) "aaa"
    assertEqual "Should parse 'aa' until 'b'" (Just (['a', 'a'], "b")) result1
    assertEqual "Should parse all 'a's without 'b'" (Just (['a', 'a', 'a'], "")) result2

-- Test pour la fonction parseString
testParseString :: Test
testParseString = TestCase $ do
    let result1 = runParser (parseString "hello") "hello world"
    let result2 = runParser (parseString "hello") "hel"
    assertEqual "Should parse 'hello'" (Just ("hello", " world")) result1
    assertEqual "Should fail for incomplete match" Nothing result2

-- Test pour la fonction parseWhitespace
testParseWhitespace :: Test
testParseWhitespace = TestCase $ do
    let result1 = runParser parseWhitespace "   hello"
    let result2 = runParser parseWhitespace "hello"
    assertEqual "Should parse spaces correctly" (Just ((), "hello")) result1
    assertEqual "Should fail if no whitespace" Nothing result2

-- Test pour la fonction parseEof
testParseEof :: Test
testParseEof = TestCase $ do
    let result1 = runParser parseEof ""
    let result2 = runParser parseEof "hello"
    assertEqual "Should succeed at EOF" (Just ((), "")) result1
    assertEqual "Should fail if not EOF" Nothing result2

-- Test pour la fonction parseSatisfy
testParseSatisfy :: Test
testParseSatisfy = TestCase $ do
    let result1 = runParser (parseSatisfy isDigit) "1"  -- Test avec un chiffre
    let result2 = runParser (parseSatisfy isDigit) "a"  -- Test avec un caractère non-chiffre
    assertEqual "Should parse '1' as digit" (Just ('1', "")) result1
    assertEqual "Should fail for non-digit 'a'" Nothing result2

-- Test pour la fonction parseSepBy
testParseSepBy :: Test
testParseSepBy = TestCase $ do
    let result1 = runParser (parseSepBy (parseChar 'a') (parseChar ',')) "a,a,a"
    let result2 = runParser (parseSepBy (parseChar 'a') (parseChar ',')) "a"
    assertEqual "Should parse 'a,a,a' into ['a', 'a', 'a']" (Just (['a', 'a', 'a'], "")) result1
    assertEqual "Should parse 'a' into ['a']" (Just (['a'], "")) result2

-- Test pour la fonction parseOpener
testParseOpener :: Test
testParseOpener = TestCase $ do
    let result1 = runParser (parseOpener '(') "( )"
    let result2 = runParser (parseOpener '(') " "
    assertEqual "Should parse opener '('" (Just ((), " )")) result1
    assertEqual "Should fail for incorrect opener" Nothing result2

-- Test pour la fonction parseCloser
testParseCloser :: Test
testParseCloser = TestCase $ do
    let result1 = runParser (parseCloser ')') " )"
    let result2 = runParser (parseCloser ')') " "
    assertEqual "Should parse closer ')'" (Just ((), "")) result1
    assertEqual "Should fail for incorrect closer" Nothing result2

-- Test pour parseExpo et parseNumber
testParseExpoAndNumber :: Test
testParseExpoAndNumber = TestCase $ do
    let result1 = runParser parseExpo "3e10"  -- Test pour exponentiation
    let result2 = runParser parseNumber "-3.14"  -- Test pour un nombre flottant
    let result3 = runParser parseExpo "2.5e-3"  -- Test pour un autre nombre exponentiel
    assertEqual "Should parse exponent correctly" (Just (3.0e10, "")) result1
    assertEqual "Should parse negative number" (Just (-3.14, "")) result2
    assertEqual "Should parse scientific notation" (Just (2.5e-3, "")) result3

-- Test pour _parseNumber
testParseNumber :: Test
testParseNumber = TestCase $ do
    -- Test de nombres positifs et négatifs
    let result1 = runParser (parseNumber) "123"  -- Nombre entier positif
    let result2 = runParser (parseNumber) "-123"  -- Nombre entier négatif
    let result3 = runParser (parseNumber) "3.14"  -- Nombre avec décimale
    let result4 = runParser (parseNumber) "-3.14"  -- Nombre négatif avec décimale

    -- Test pour nombres exponentiels
    let result5 = runParser (parseNumber) "1e2"  -- Notation scientifique positive
    let result6 = runParser (parseNumber) "2.5e-3"  -- Notation scientifique avec décimal et exposant négatif
    let result7 = runParser (parseNumber) "-2.5E3"  -- Notation scientifique avec exposant

    -- Test de zéro
    let result8 = runParser (parseNumber) "0"  -- Zéro
    let result9 = runParser (parseNumber) "0.0"  -- Zéro flottant

    -- Test pour cas invalides
    let result10 = runParser (parseNumber) "abc"  -- Entrée invalide

    -- Vérifier les résultats valides
    assertEqual "Should parse '123' as 123" (Just (123.0, "")) result1
    assertEqual "Should parse '-123' as -123" (Just (-123.0, "")) result2
    assertEqual "Should parse '3.14' correctly" (Just (3.14, "")) result3
    assertEqual "Should parse '-3.14' correctly" (Just (-3.14, "")) result4
    assertEqual "Should parse '1e2' correctly" (Just (1.0e2, "")) result5
    assertEqual "Should parse '2.5e-3' correctly" (Just (2.5e-3, "")) result6
    assertEqual "Should parse '-2.5E3' correctly" (Just (-2.5e3, "")) result7
    assertEqual "Should parse '0' as 0" (Just (0.0, "")) result8
    assertEqual "Should parse '0.0' as 0.0" (Just (0.0, "")) result9

    -- Vérifier les erreurs
    assertEqual "Should fail for 'abc'" Nothing result10

-- Test pour la fonction spaces
testSpaces :: Test
testSpaces = TestCase $ do
    -- Test avec des espaces
    let result1 = runParser spaces "   hello"  -- Plusieurs espaces
    let result2 = runParser spaces "\t\n"  -- Tabulation et nouvelle ligne

    -- Test sans espaces
    let result3 = runParser spaces "hello"  -- Aucun espace, chaîne sans espace blanc

    -- Vérifier les résultats
    assertEqual "Should parse spaces correctly" (Just ((), "hello")) result1
    assertEqual "Should parse tab and newline correctly" (Just ((), "hello")) result2
    assertEqual "Should fail if no spaces are present" Nothing result3

-- Regroupement des tests
testNewParser :: Test
testNewParser = TestList
    [ testParseAny
    , testParseChar
    , testParseAnyChar
    , testParseManyUntil
    , testParseString
    , testParseWhitespace
    , testParseEof
    , testParseSatisfy
    , testParseSepBy
    , testParseOpener
    , testParseCloser
    , testParseExpoAndNumber
    , testParseNumber
    , testSpaces
    ]
