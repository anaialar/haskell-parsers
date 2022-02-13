module Main (main) where

import qualified Parsers.Json (parseJson)

main :: IO ()
main = do
  jsonTestData <- readFile "test/jsonTestData.json"
  putStrLn $ show $ Parsers.Json.parseJson jsonTestData
