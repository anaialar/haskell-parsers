module Main (main) where

import qualified Parsers.Xml (parseXml, formattedStringifyXml)
import qualified Parsers (toJson)
import qualified Testing (runTests, success, failure)

xmlTestData = readFile "test/xmlTestData.xml"

xmlParsingTest :: IO Bool
xmlParsingTest = do
  xmlTestData' <- xmlTestData
  case Parsers.Xml.parseXml xmlTestData' of
    Right (result, _) -> do
      putStrLn $ show result
      Testing.success "Xml parsing test successful"
    Left r -> do
      putStrLn $ show r
      Testing.failure "Xml parsing test failed"

xmlToJsonConversionTest :: IO Bool
xmlToJsonConversionTest = do
  xmlTestData' <- xmlTestData
  case Parsers.Xml.parseXml xmlTestData' of
    Right (result, _) -> do
      putStrLn $ show $ Parsers.toJson result
      Testing.success "Xml conversion test successful"
    Left r -> do
      putStrLn $ show r
      Testing.failure "Xml conversion test failed"

main :: IO ()
main = Testing.runTests [xmlParsingTest, xmlToJsonConversionTest]
