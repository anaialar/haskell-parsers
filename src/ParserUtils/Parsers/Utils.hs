module Parsers.Utils (
  Predicate,
  ParsingError,
  ParsingResult,
  ParsingResponse,
  ParsedNumber,
  Parser,

  trimAllSpaces,
  trimSpaces,

-- Predicate utilities
  (<||>),
  (<&&>),
  pNot,

-- Error handlers export

  unhandledParsingError,
  unexpectedTokenError,
  badToken,

-- Parsing utilities export
  parseCharacter,
  parseString,
  parseNumber,
  parseBool,
  matchCharacter,
  matchCharacterIgnoringSpaces,
  matchString,
  matchStringIgnoringTrailingSpaces
  ) where

import Data.Char (isSpace, isDigit)
import Data.Number.CReal (CReal)

type Predicate a = a -> Bool

type ParsedNumber = CReal

type ParsingError = (Int, String)
type ParsingResult a = (a, String)
type ParsingResponse a = Either ParsingError (ParsingResult a)
type Parser a = String -> ParsingResponse a

trimSpaces :: String -> String
trimSpaces = dropWhile isSpace . trimTrailingSpaces
  where
    trimTrailingSpaces :: String -> String
    trimTrailingSpaces [] = []
    trimTrailingSpaces y@[x] = if isSpace x then [] else y
    trimTrailingSpaces (x:xs)
      | null ys && (isSpace x) = ys
      | otherwise = x : ys
      where
        ys = trimTrailingSpaces xs

trimAllSpaces :: String -> String
trimAllSpaces xs = [x | x <- xs, not $ isSpace x]

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

unexpectedTokenError :: String -> String -> Int -> ParsingResponse a
unexpectedTokenError x y p = Left (p, "Unexpected character `" ++ x ++ "`. Expected `" ++ y ++ "`.")

badToken :: Int -> ParsingResponse a
badToken p = Left (p, "Bad token at " ++ (show p))

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

parseNumber :: Parser CReal
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
matchCharacter x [] = unexpectedTokenError "\0" [x] $ -1
matchCharacter x xs@(y:_) =
  case parseCharacter (==x) xs of
    Left _ -> unexpectedTokenError [y] [x] $ -1
    result -> result

matchCharacterIgnoringSpaces :: Char -> Parser Char
matchCharacterIgnoringSpaces x [] = unexpectedTokenError "\0" [x] $ -1
matchCharacterIgnoringSpaces x xs@(y:ys)
  | isSpace y = matchCharacterIgnoringSpaces x ys
  | otherwise = matchCharacter x xs

matchString :: String -> Parser String
matchString xs = matchString' xs
  where
    matchString' [] ys = return (xs, ys)
    matchString' _ [] = unexpectedTokenError xs "\0" $ -1
    matchString' (x:xs) (y:ys)
      | x == y = matchString' xs ys
      | otherwise = unhandledParsingError

matchStringIgnoringTrailingSpaces :: String -> Parser String
matchStringIgnoringTrailingSpaces xs [] = unexpectedTokenError xs "\0" $ -1
matchStringIgnoringTrailingSpaces xs ys@(y:ys')
  | isSpace y = matchStringIgnoringTrailingSpaces xs ys'
  | otherwise = matchString xs ys
