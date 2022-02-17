module Main (main) where

import qualified Parsers.Xml (parseXml, stringifyXml)
import qualified Testing (runTests, success, failure)

xmlParsingTest :: IO Bool
xmlParsingTest = do
  xmlTestData <- readFile "test/xmlTestData.xml"
  case Parsers.Xml.parseXml xmlTestData of
    Right (result, _) -> do
      putStrLn $ Parsers.Xml.stringifyXml result
      Testing.success "Xml"
    Left r -> do
      putStrLn $ show r
      Testing.failure "Xml"

main :: IO ()
main = Testing.runTests [xmlParsingTest]
