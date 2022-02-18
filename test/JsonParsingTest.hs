module Main (main) where

import qualified Parsers.Json (parseJson, stringifyJson)
import qualified Parsers (Convertable, toXml)
import qualified Testing (runTests, success, failure)

jsonParsingTest :: IO Bool
jsonParsingTest = do
  jsonTestData <- readFile "test/jsonTestData.json"
  case Parsers.Json.parseJson jsonTestData of
    Right (result, _) -> do
      putStrLn $ show $ Parsers.toXml result
      Testing.success "Json"
    Left r -> Testing.failure "Json"

main :: IO ()
main = Testing.runTests [jsonParsingTest]
