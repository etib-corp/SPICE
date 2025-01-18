module GLPInterface where

import MyParser
import Structures
import Files
import Lib
import GLP

-- Parse a string with a given configuration. Mainly used for testing purposes.
parseWithConfig :: String -> ParserConfig -> IO ()
parseWithConfig s pc = case parse s (parseExpressionConfig pc) of
      Left err -> putStrLn $ show err
      Right result -> print result

-- Check if the file has the right extension.
isInGoodExtension :: String -> Bool
isInGoodExtension "" = False
isInGoodExtension str = case length str == 6 of
    True -> str == ".spice"
    False -> isInGoodExtension $ tail str

-- Extract the configuration from a raw string.
getParserConfiguration :: String -> Maybe ParserConfig
getParserConfiguration "" = Nothing
getParserConfiguration str = parseSyntaxConfiguration $ lines str

-- Used to manage error cases (If the configuration is not valid, it returns a NullConfig).
testParserConfiguration :: String -> IO ParserConfig
testParserConfiguration content = case getParserConfiguration content of
    Just config -> pure config
    Nothing -> pure NullConfig

-- Interface to load a parser configuration from a file path given as argument.
loadParserConfiguration :: String -> IO ParserConfig
loadParserConfiguration path = case isInGoodExtension path of
    True -> secureGetContent path >>= testParserConfiguration
    False -> pure NullConfig
