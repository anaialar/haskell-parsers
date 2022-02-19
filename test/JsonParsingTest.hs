module Main (main) where

import qualified Parsers.Json (parseJson, stringifyJson)
import qualified Parsers (toXml)
import qualified Testing (runTests, success, failure)

jsonTestData = readFile "test/jsonTestData.json"

jsonParsingTest :: IO Bool
jsonParsingTest = do
  jsonTestData' <- jsonTestData
  case Parsers.Json.parseJson jsonTestData' of
    Right (result, _) -> do
      putStrLn $ show result
      Testing.success "Json parsing test successful"
    Left r -> do
      putStrLn $ show r
      Testing.failure "Json parsing test failed"

jsonToXmlConversionTest :: IO Bool
jsonToXmlConversionTest = do
  jsonTestData' <- jsonTestData
  case Parsers.Json.parseJson jsonTestData' of
    Right (result, _) -> do
      putStrLn $ show $ Parsers.toXml result
      Testing.success "Json to xml conversion test successful"
    Left r -> do
      putStrLn $ show r
      Testing.failure "Json to xml conversion test failed"

main :: IO ()
main = Testing.runTests [jsonParsingTest, jsonToXmlConversionTest]
