module Main (main) where

import qualified Parsers.Json (parseJson, stringifyJson)

main :: IO ()
main = do
  jsonTestData <- readFile "test/jsonTestData.json"
  case Parsers.Json.parseJson jsonTestData of
    Right (result, _) -> putStrLn $ show result
    Left r -> fail $ show r
