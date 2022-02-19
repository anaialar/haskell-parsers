module Parsers.Json (
  JsonData (JsonNull, JsonNumber, JsonBool, JsonString, JsonArray, JsonObject),
  stringifyJson,
  formattedStringifyJson,
  parseJson
  ) where

import Data.Char (isSpace)
import Data.Map.Strict (Map, insert, empty, toList, fromList)
import Parsers.Utils (
  Parser,
  ParsedNumber,
  (<&&>),
  unhandledParsingError,
  badToken,
  parseNumber,
  parseBool,
  parseString,
  matchString,
  matchCharacterIgnoringSpaces
  )

import Parsers.Utils.Stringify (
  Stringifier,
  FormattedStringifier,
  getIndentation
  )
import Parsers.Xml (
  XmlData (XmlString, XmlElement)
  )

data JsonData = JsonNull
              | JsonNumber ParsedNumber
              | JsonBool Bool
              | JsonString String
              | JsonArray [JsonData]
              | JsonObject (Map String JsonData)

instance Show JsonData where
  show = formattedStringifyJson "    "

stringifyJson :: Stringifier JsonData
stringifyJson (JsonNull) = "null"
stringifyJson (JsonNumber d) = show d
stringifyJson (JsonBool True) = "true"
stringifyJson (JsonBool False) = "false"
stringifyJson (JsonString xs) = '"' : (xs ++ "\"")

stringifyJson (JsonArray xs) = stringifyJsonArray "[" xs
  where
    stringifyJsonArrayEntry :: String -> String -> Stringifier JsonData
    stringifyJsonArrayEntry acc e x = acc ++ (stringifyJson x) ++ e

    stringifyJsonArray :: String -> Stringifier [JsonData]
    stringifyJsonArray _ [] = "[]"
    stringifyJsonArray acc [x] = stringifyJsonArrayEntry acc "]" x
    stringifyJsonArray acc (x:xs) =
      let
        acc' = stringifyJsonArrayEntry acc "," x
      in stringifyJsonArray acc' xs

stringifyJson (JsonObject xs) = stringifyJsonObject "{" $ toList xs
  where
    stringifyJsonObjectProperty :: String -> String -> Stringifier (String, JsonData)
    stringifyJsonObjectProperty acc e (k, v) = acc ++ "\"" ++ k ++ "\":" ++ (stringifyJson v) ++ e

    stringifyJsonObject :: String -> Stringifier [(String, JsonData)]
    stringifyJsonObject _ [] = "{}"
    stringifyJsonObject acc [x] = stringifyJsonObjectProperty acc "}" x
    stringifyJsonObject acc (x:xs) =
      let
        acc' = stringifyJsonObjectProperty acc "," x
      in stringifyJsonObject acc' xs

formattedStringifyJson :: FormattedStringifier JsonData
formattedStringifyJson i = formattedStringifyJson' 0
  where
    getIndentation' = getIndentation i

    formattedStringifyJson' :: Int -> JsonData -> String
    formattedStringifyJson' i (JsonArray xs) = formattedStringifyJsonArray "[\n" xs
      where
        i' = getIndentation' (i + 1)

        formattedStringifyJsonArrayEntry :: String -> String -> Stringifier JsonData
        formattedStringifyJsonArrayEntry acc e x = acc ++ i' ++ (formattedStringifyJson' (i + 1) x) ++ e

        formattedStringifyJsonArray :: String -> [JsonData] -> String
        formattedStringifyJsonArray _ [] = "[]"
        formattedStringifyJsonArray acc [x] = formattedStringifyJsonArrayEntry acc ("\n" ++ (getIndentation' i) ++ "]") x
        formattedStringifyJsonArray acc (x:xs) =
          let
            acc' = formattedStringifyJsonArrayEntry acc ",\n" x
          in formattedStringifyJsonArray acc' xs

    formattedStringifyJson' i (JsonObject xs) = formattedStringifyJsonObject "{\n" $ toList xs
      where
        i' = getIndentation' (i + 1)
        formattedStringifyJsonObjectProperty :: String -> String -> Stringifier (String, JsonData)
        formattedStringifyJsonObjectProperty acc e (k, v) = acc ++ i' ++ "\"" ++ k ++ "\": " ++ (formattedStringifyJson' (i + 1) v) ++ e

        formattedStringifyJsonObject :: String -> Stringifier [(String, JsonData)]
        formattedStringifyJsonObject _ [] = "{}"
        formattedStringifyJsonObject acc [x] = formattedStringifyJsonObjectProperty acc ("\n" ++ (getIndentation' i) ++ "}") x
        formattedStringifyJsonObject acc (x:xs) =
          let
            acc' = formattedStringifyJsonObjectProperty acc ",\n" x
          in formattedStringifyJsonObject acc' xs

    formattedStringifyJson' _ x = stringifyJson x

parseJsonValue :: Parser JsonData
parseJsonValue ('"':xs) = do
  (result, rest) <- parseString (/='"') xs
  return (JsonString result, tail rest)

parseJsonValue xs =
  case parseNumber xs of
    Left _ ->
      case parseBool "true" "false" xs of
        Left _ -> do
          case matchString "null" xs of
            Left _ -> badToken (-1)
            Right (_, rest) -> return (JsonNull, rest)
        Right (result, rest) -> return (JsonBool result, rest)
    Right (result, rest) -> return (JsonNumber result, rest)

parseJsonObjectProperty :: Parser (String, JsonData)
parseJsonObjectProperty ('"':xs) = do
  (key, rest) <- parseString ((/='"') <&&> (/='\t')) xs
  (_, rest) <- matchCharacterIgnoringSpaces ':' $ tail rest
  (value, rest) <- parseJson rest
  return ((key, value), rest)

parseJsonObjectProperty (x:xs)
  | isSpace x = parseJsonObjectProperty xs
  | otherwise = unhandledParsingError

parseJsonObject :: Parser JsonData
parseJsonObject = parseJsonObject' empty
  where
    parseJsonObject' :: Map String JsonData -> Parser JsonData
    parseJsonObject' _ "" = unhandledParsingError
    parseJsonObject' acc xs =
      case parseJsonObjectProperty xs of
        Left _ -> badToken (-1)
        Right ((key, value), rest) ->
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
