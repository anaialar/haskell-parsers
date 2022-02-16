module Parsers.Utils.Stringify (
  Stringifier,
  FormattedStringifier,
  getIndentation
  ) where

type Stringifier a = a -> String
type FormattedStringifier a = String -> Stringifier a

getIndentation :: String -> Int -> String
getIndentation indentation x = concat $ map (const indentation) [1..x]
