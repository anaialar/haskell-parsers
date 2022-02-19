module Testing (
  success,
  failure,
  runTests
  ) where

import System.Exit (exitFailure)

testResult :: Bool -> String -> IO Bool
testResult s xs = do
  putStrLn xs
  return s

success :: String -> IO Bool
success = testResult True . ("✔ "++) . (++".")

failure :: String -> IO Bool
failure = testResult True . ("❌ "++) . (++".")

testIsSuccessful :: [IO Bool] -> IO Bool
testIsSuccessful [] = return True
testIsSuccessful (x:xs) = do
  y <- x
  if y
    then testIsSuccessful xs
    else return y

runTests :: [IO Bool] -> IO ()
runTests tests = do
  success <- testIsSuccessful tests
  if success
    then return ()
    else exitFailure
