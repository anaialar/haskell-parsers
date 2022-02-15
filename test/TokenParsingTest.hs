module Main (main) where

import System.Random (randomRIO)
import qualified Parsers (parseNumber, parseBool)
import qualified Testing (runTests, success, failure)

numberParsingTest :: IO Bool
numberParsingTest = do
  num <- randomRIO (-696969.420420, 696969.420420) :: IO Double
  let response = Parsers.parseNumber (show num)
  case response of
    Right (result, _)
      | result == num -> Testing.success "Number"
    _ -> Testing.failure "Number"

boolParsingText :: IO Bool
boolParsingText = do
  num <- randomRIO (1, 2) :: IO Double
  let x = num > 1.5
  let response = Parsers.parseBool "True" "False" (show x)
  case response of
    Right (result, _)
      | result == x -> Testing.success "Bool"
    _ -> Testing.failure "Bool"

main :: IO ()
main = Testing.runTests [boolParsingText, numberParsingTest]
