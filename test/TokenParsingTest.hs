module Main (main) where

import System.Random (randomRIO)
import System.Exit (exitFailure)
import qualified Parsers (parseNumber, parseBool)
import Testing (testIsSuccessful, success, failure)

numberParsingTest :: IO Bool
numberParsingTest = do
  num <- randomRIO (-696969.420420, 696969.420420) :: IO Double
  let response = Parsers.parseNumber (show num)
  case response of
    Right (result, _)
      | result == num -> success "Number parsing test successful."
    _ -> failure "Number parsing test failed."

boolParsingText :: IO Bool
boolParsingText = do
  num <- randomRIO (1, 2) :: IO Double
  let x = num > 1.5
  let response = Parsers.parseBool "True" "False" (show x)
  case response of
    Right (result, _)
      | result == x -> success "Bool parsing test successful."
    _ -> failure "Bool parsing test failed."

main :: IO ()
main = do
  let tests = [boolParsingText, numberParsingTest]
  success <- testIsSuccessful tests
  if success
    then return ()
    else exitFailure
