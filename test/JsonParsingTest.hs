module Main (main) where

import qualified Parsers.Json (parseJson)

main :: IO ()
main = do
  putStrLn $ show $ Parsers.Json.parseJson "{\"name\": \"arpitchakladar\", \"info\": { \"male\": true }, \"hobbies\": [\"nothing\", \"nothing2\", null]}"
