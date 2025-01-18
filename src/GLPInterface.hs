module GLPInterface where

import MyParser
import Structures
import Files
import Lib
import GLP

parseWithConfig :: String -> ParserConfig -> IO ()
parseWithConfig s pc = case parse s (parseExpressionConfig pc) of
      Left err -> putStrLn $ show err
      Right result -> print result

isInGoodExtension :: String -> Bool
isInGoodExtension "" = False
isInGoodExtension str = case length str == 6 of
    True -> str == ".spice"
    False -> isInGoodExtension $ tail str

getParserConfiguration :: String -> Maybe ParserConfig
getParserConfiguration "" = Nothing
getParserConfiguration str = parseSyntaxConfiguration $ lines str

testParserConfiguration :: String -> IO ParserConfig
testParserConfiguration content = case getParserConfiguration content of
    Just config -> pure config
    Nothing -> pure NullConfig

loadParserConfiguration :: String -> IO ParserConfig
loadParserConfiguration path = secureGetContent path >>= testParserConfiguration
