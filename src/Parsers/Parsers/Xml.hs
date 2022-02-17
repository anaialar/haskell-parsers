module Parsers.Xml (
  parseXml
  ) where

import Data.Char (isSpace, isLetter)
import Data.Map.Strict (Map, insert, empty, toList)
import Parsers.Utils (
  Parser,
  (<||>),
  pNot,
  unhandledParsingError,
  unexpectedTokenError,
  parseString,
  matchCharacterIgnoringSpaces,
  matchStringIgnoringTrailingSpaces
  )

data XmlElement = XmlElement {
  tag :: String,
  attributes :: (Map String String),
  children :: Maybe [XmlData]
} deriving (Show)

data XmlData = XmlString String | XmlDataElement XmlElement deriving(Show)

parseXmlElementAttributes :: Parser ((Map String String), Bool)
parseXmlElementAttributes = parseXmlElementAttributes' empty
  where
    parseXmlElementAttributes' :: Map String String -> Parser ((Map String String), Bool)
    parseXmlElementAttributes' acc [] = unexpectedTokenError "\0" ">" $ -1
    parseXmlElementAttributes' acc xs@(y:ys)
      | isSpace y = parseXmlElementAttributes ys
      | y == '>' = return ((acc, True), ys)
      | y == '/' && (head ys == '>') = return ((acc, False), tail ys)
      | otherwise = do
        (key', rest') <- parseString (/='=') xs
        let key = foldr (\x acc -> if isSpace x then acc else x : acc) [] key'
        (_, rest) <- matchCharacterIgnoringSpaces '"' $ tail rest'
        (value, rest') <- parseString (/='"') rest
        parseXmlElementAttributes' (insert key value acc) $ tail rest'

parseXmlElementChildren :: String -> Parser [XmlData]
parseXmlElementChildren tag = parseXmlElementChildren' []
  where
    endTag = "</" ++ tag ++ ">"

    parseXmlElementChildren' :: [XmlData] -> Parser [XmlData]
    parseXmlElementChildren' _ [] = unexpectedTokenError endTag "\0" $ -1
    parseXmlElementChildren' acc xs =
      case matchStringIgnoringTrailingSpaces endTag xs of
        Left _ -> do
          (child, rest) <- parseXml xs
          parseXmlElementChildren' (child : acc) rest

        Right (_, rest') -> return (acc, rest')

parseXmlElement :: Parser XmlData
parseXmlElement xs = do
  (tag, rest) <- parseString (isLetter <||> (=='-')) xs
  ((attributes, hasChildren), rest') <- parseXmlElementAttributes rest
  let
    f = do
      if hasChildren
        then do
          (children, rest) <- parseXmlElementChildren tag rest'
          return (Just children, rest)
        else return (Nothing, rest')
  (children, rest) <- f
  let element = XmlElement {
    tag = tag,
    attributes = attributes,
    children = children
  }
  return (XmlDataElement element, rest)

parseXmlString :: Parser XmlData
parseXmlString xs = do
  (ys, rest) <- parseString (/='<') xs
  return (XmlString ys, rest)

parseXml :: Parser XmlData
parseXml [] = unhandledParsingError
parseXml ys@(x:xs)
  | isSpace x = parseXml xs
  | x == '<' && (not $ isSpace $ head xs) = parseXmlElement xs
  | otherwise = parseXmlString ys
