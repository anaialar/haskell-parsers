module Main (main) where

import System.Random (randomIO)
import System.Exit (exitFailure)
import qualified Parsers (parseNumber)
import Testing (testIsSuccessful, success, failure)

numberParsingTest :: IO Bool
numberParsingTest = do
  num <- randomIO :: IO Double
  let response = Parsers.parseNumber (show num)
  case response of
    Right (result, _)
      | result == num -> success "Number parsing test successful."
    _ -> failure "Number parsing test failed."

main :: IO ()
main = do
  let tests = [numberParsingTest]
  success <- testIsSuccessful tests
  if success
    then return ()
    else exitFailure
