module Testing (
  success,
  failure,
  testIsSuccessful
  ) where

testResult :: Bool -> String -> IO Bool
testResult s xs = do
  putStrLn xs
  return s

success :: String -> IO Bool
success = testResult True . ("✔ "++)

failure :: String -> IO Bool
failure = testResult True . ("❌ "++)

testIsSuccessful :: [IO Bool] -> IO Bool
testIsSuccessful [] = return True
testIsSuccessful (x:xs) = do
  y <- x
  if y
    then testIsSuccessful xs
    else return y
