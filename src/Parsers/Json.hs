module Parsers.Json (
  JsonData,
  parseJson
  ) where

import Data.Char (isSpace, isDigit)
import Data.Map.Strict (Map, insert, empty)
import Parsers (
  Parser,
  unhandledParsingError,
  parseNumber,
  parseBool,
  parseString,
  matchString,
  matchCharacterIgnoringSpaces
  )

data JsonData = JsonNull
              | JsonNumber Double
              | JsonBool Bool
              | JsonString String
              | JsonArray [JsonData]
              | JsonObject (Map String JsonData) deriving (Show)

parseJsonValue :: Parser JsonData
parseJsonValue ('"':xs) = do
  (result, rest) <- parseString (/='"') xs
  return (JsonString result, tail rest)

parseJsonValue xs =
  case parseNumber xs of
    Left _ ->
      case parseBool "true" "false" xs of
        Left _ -> do
          (result, rest) <- matchString "null" xs
          return (JsonNull, rest)
        Right (result, rest) -> return (JsonBool result, rest)
    Right (result, rest) -> return (JsonNumber result, rest)

parseJsonObjectProperty :: Parser (String, JsonData)
parseJsonObjectProperty ('"':xs) = do
  (key, rest') <- parseString (/='"') xs
  (_, rest') <- matchCharacterIgnoringSpaces ':' $ tail rest'
  (value, rest) <- parseJson rest'
  return ((key, value), rest)

parseJsonObjectProperty (x:xs)
  | isSpace x = parseJsonObjectProperty xs
  | otherwise = unhandledParsingError

parseJsonObject :: Parser JsonData
parseJsonObject = parseJsonObject' empty
  where
    parseJsonObject' :: Map String JsonData -> Parser JsonData
    parseJsonObject' _ "" = unhandledParsingError
    parseJsonObject' acc xs = do
      ((key, value), rest) <- parseJsonObjectProperty xs
      case matchCharacterIgnoringSpaces ',' rest of
        Left _ -> do
          (_, rest') <- matchCharacterIgnoringSpaces '}' rest
          return (JsonObject $ insert key value acc, rest')
        Right (_, rest') -> parseJsonObject' (insert key value acc) rest'

parseJsonArray :: Parser JsonData
parseJsonArray = parseJsonArray' []
  where
    parseJsonArray' :: [JsonData] -> Parser JsonData
    parseJsonArray' _ "" = unhandledParsingError
    parseJsonArray' acc xs = do
      (result, rest) <- parseJson xs
      case matchCharacterIgnoringSpaces ',' rest of
        Left _ -> do
          (_, rest') <- matchCharacterIgnoringSpaces ']' rest
          return (JsonArray $ result : acc, rest')
        Right (_, rest') -> parseJsonArray' (result : acc) rest'

parseJson :: Parser JsonData
parseJson ('{':xs) = parseJsonObject xs
parseJson ('[':xs) = parseJsonArray xs
parseJson xs@(y:ys)
  | isSpace y = parseJson ys
  | otherwise = parseJsonValue xs
