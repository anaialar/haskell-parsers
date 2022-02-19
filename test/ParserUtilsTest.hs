module Main (main) where

import System.Random (randomRIO)
import qualified Parsers.Utils (ParsedNumber, parseNumber, parseBool)
import qualified Testing (runTests, success, failure)

numberParsingTest :: IO Bool
numberParsingTest = do
  num' <- randomRIO (-696969.420420, 696969.420420) :: IO Double
  let num = fromRational $ toRational num' :: Parsers.Utils.ParsedNumber
  let response = Parsers.Utils.parseNumber (show num)
  case response of
    Right (result, _)
      | result == num -> Testing.success "Number parsing test successful"
    _ -> Testing.failure "Number parsing test failed"

boolParsingText :: IO Bool
boolParsingText = do
  num' <- randomRIO (1, 2) :: IO Double
  let num = fromRational $ toRational num' :: Parsers.Utils.ParsedNumber
  let x = num > 1.5
  let response = Parsers.Utils.parseBool "True" "False" (show x)
  case response of
    Right (result, _)
      | result == x -> Testing.success "Bool parsing test successful"
    _ -> Testing.failure "Bool parsing test failed"

main :: IO ()
main = Testing.runTests [boolParsingText, numberParsingTest]
