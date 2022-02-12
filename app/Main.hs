module Main where

import qualified Parsers (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Parsers.someFunc
