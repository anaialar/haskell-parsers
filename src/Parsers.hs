module Parsers (
  Predicate,
  ParsingError,
  ParsingResult,
  ParsingResponse,
  Parser,

-- Predicate utilities
  (<||>),
  (<&&>),
  pNot,

-- Error handlers export

  unhandledParsingError,
  characterParsingError,

-- Parsing utilities export
  parseCharacter,
  parseString,
  parseNumber,
  parseBool,
  matchCharacter,
  matchCharacterIgnoringSpaces,
  matchString
  ) where

import Data.Char (isSpace, isDigit)

type Predicate a = a -> Bool

type ParsingError = (Int, String)
type ParsingResult a = (a, String)
type ParsingResponse a = Either ParsingError (ParsingResult a)
type Parser a = String -> ParsingResponse a

-- Predicate utilities
(<||>) :: Predicate a -> Predicate a -> Predicate a
(<||>) predicateA predicateB x = predicateA x || (predicateB x)

(<&&>) :: Predicate a -> Predicate a -> Predicate a
(<&&>) predicateA predicateB x = predicateA x && (predicateB x)

pNot :: Predicate a -> Predicate a
pNot predicateA = not . predicateA

-- Error handlers
unhandledParsingError :: ParsingResponse a
unhandledParsingError = Left (-1, "")

characterParsingError :: Char -> Char -> Int -> ParsingResponse a
characterParsingError x y p = Left (p, "Unexpected character `" ++ [x] ++ "`. Expected `" ++ [y] ++ "`.")

-- Parsing utilities
parseCharacter :: Predicate Char -> Parser Char
parseCharacter _ [] = unhandledParsingError
parseCharacter p (x:xs)
  | p x = return (x, xs)
  | otherwise = unhandledParsingError

parseString :: Predicate Char -> Parser String
parseString p = return . parseString'
  where
    parseString' :: String -> (String, String)
    parseString' "" = ("", "")
    parseString' ys@(x:xs)
      | p x = let
          (result, rest) = parseString' xs
        in (x : result, rest)
      | otherwise = ("", ys)

parseNumber :: Parser Double
parseNumber xs@(y:ys)
  | isDigit y = do
    (result, rest) <- parseNumber' False xs
    return (read result, rest)
  | y == '-' && (isDigit $ head ys) = do
    (result, rest) <- parseNumber' False ys
    return (-(read result), rest)
  | otherwise = unhandledParsingError
  where
    parseNumber' :: Bool -> Parser String
    parseNumber' _ "" = return ("", "")
    parseNumber' hasSeperator ys@(x:xs)
      | isDigit x = do
        (result, rest) <- parseNumber' hasSeperator xs
        return (x : result, rest)
      | x == '.' =
        if hasSeperator
          then unhandledParsingError
          else do
            (result, rest) <- parseNumber' hasSeperator xs
            return (x : result, rest)
      | otherwise = return ("", ys)

parseBool :: String -> String -> Parser Bool
parseBool txs fxs xs =
  case matchString txs xs of
    Left _ -> do
      (_, rest) <- matchString fxs xs
      return (False, rest)

    Right (_, rest) -> return (True, rest)

matchCharacter :: Char -> Parser Char
matchCharacter x [] = characterParsingError '\0' x $ -1
matchCharacter x xs@(y:_) =
  case parseCharacter (==x) xs of
    Left _ -> characterParsingError y x $ -1
    result -> result

matchCharacterIgnoringSpaces :: Char -> Parser Char
matchCharacterIgnoringSpaces x [] = characterParsingError '\0' x $ -1
matchCharacterIgnoringSpaces x xs@(y:ys)
  | isSpace y = matchCharacterIgnoringSpaces x ys
  | otherwise = matchCharacter x xs

matchString :: String -> Parser String
matchString xs = matchString' xs
  where
    matchString' [] ys = return (xs, ys)
    matchString' _ [] = unhandledParsingError
    matchString' (x:xs) (y:ys)
      | x == y = matchString' xs ys
      | otherwise = unhandledParsingError
