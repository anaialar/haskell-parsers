module Parsers.Xml (
  XmlData (XmlString, XmlElement),
  parseXml,
  stringifyXml,
  formattedStringifyXml
  ) where

import Data.Char (isSpace, isLetter, isDigit)
import Data.Map.Strict (Map, insert, empty, toList)
import Parsers.Utils (
  Parser,
  (<||>),
  pNot,
  unhandledParsingError,
  unexpectedTokenError,
  parseString,
  parseCharacter,
  matchCharacterIgnoringSpaces,
  matchStringIgnoringTrailingSpaces
  )

import Parsers.Utils.Stringify (
  Stringifier,
  FormattedStringifier,
  getIndentation
  )

data XmlData = XmlString String
             | XmlElement {
                tag :: String,
                attributes :: (Map String String),
                children :: Maybe [XmlData]
              }

instance Show XmlData where
  show = formattedStringifyXml "    "

stringifyXmlElementAttributes :: Stringifier (Map String String)
stringifyXmlElementAttributes = foldl f "" . toList
  where
    f acc (key, value) = " " ++ key ++ "=" ++ value ++ acc

stringifyXml :: Stringifier XmlData
stringifyXml (XmlString xs) = xs
stringifyXml (XmlElement tag attributes children) = x ++ y
  where
    x = "<" ++ tag ++ (stringifyXmlElementAttributes attributes)
    y = case children of
      Just c -> ">" ++ (concat $ map stringifyXml c) ++ "</" ++ tag ++ ">"
      Nothing -> "/>"

formattedStringifyXml :: FormattedStringifier XmlData
formattedStringifyXml i = formattedStringifyXml' 0
  where
    getIndentation' = getIndentation i

    formattedStringifyXml' :: Int -> XmlData -> String
    formattedStringifyXml' i (XmlString xs) = (getIndentation' i) ++ xs
    formattedStringifyXml' i (XmlElement tag attributes children) = ci ++ x ++ y
      where
        ci = getIndentation' i
        x = "<" ++ tag ++ (stringifyXmlElementAttributes attributes)
        y = case children of
          Just c -> ">\n" ++ (concat $ map ((++"\n") . formattedStringifyXml' (i + 1)) c) ++ ci ++ "</" ++ tag ++ ">"
          Nothing -> "/>"

validXmlElementTagFirstCharacter = isLetter <||> (=='-')

parseXmlElementAttributes :: Parser ((Map String String), Bool)
parseXmlElementAttributes = parseXmlElementAttributes' empty
  where
    parseXmlElementAttributes' :: Map String String -> Parser ((Map String String), Bool)
    parseXmlElementAttributes' _ [] = unexpectedTokenError "\0" ">" $ -1
    parseXmlElementAttributes' acc xs@(y:ys)
      | isSpace y = parseXmlElementAttributes ys
      | y == '>' = return ((acc, True), ys)
      | y == '/' && (head ys == '>') = return ((acc, False), tail ys)
      | otherwise = do
        (key', rest) <- parseString (/='=') xs
        if null rest
          then unexpectedTokenError "\0" "=" $ -1
          else do
            let key = foldr (\x acc -> if isSpace x then acc else x : acc) [] key'
            let rest' = tail rest
            (_, rest) <- matchCharacterIgnoringSpaces '"' rest'
            (value, rest') <- parseString (/='"') rest
            parseXmlElementAttributes' (insert key value acc) $ tail rest'

parseXmlElementChildren :: String -> Parser [XmlData]
parseXmlElementChildren tag = parseXmlElementChildren' []
  where
    endTag = "</" ++ tag

    parseXmlElementChildren' :: [XmlData] -> Parser [XmlData]
    parseXmlElementChildren' _ [] = unexpectedTokenError "\0" (endTag ++ ">") $ -1
    parseXmlElementChildren' acc xs =
      case matchStringIgnoringTrailingSpaces endTag xs of
        Left _ -> do
          (child, rest) <- parseXml xs
          parseXmlElementChildren' (child : acc) rest

        Right (_, rest') -> do
          (_, rest) <- matchCharacterIgnoringSpaces '>' rest'
          return (acc, rest)

parseXmlElement :: Parser XmlData
parseXmlElement xs = do
  (c, rest') <- parseCharacter validXmlElementTagFirstCharacter xs
  (tag', rest) <- parseString (validXmlElementTagFirstCharacter <||> isDigit) rest'
  let tag = c : tag'
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
  return (element, rest)

parseXmlString :: Parser XmlData
parseXmlString xs = do
  (ys, rest) <- parseXmlString' xs
  return (XmlString ys, rest)
  where
    parseXmlString' :: Parser String
    parseXmlString' xs = do
      (ys, rest) <- parseString (/='<') xs
      if null rest
        then return (ys, rest)
        else do
          let rest' = tail rest
          if (validXmlElementTagFirstCharacter <||> (=='/')) $ head rest'
            then return (ys, rest)
            else do
              (ys', rest) <- parseXmlString' rest'
              return ((ys ++ "< " ++ ys'), rest)

parseXml :: Parser XmlData
parseXml [] = unhandledParsingError
parseXml ys@(x:xs)
  | isSpace x = parseXml xs
  | x == '<' && (validXmlElementTagFirstCharacter $ head xs) = parseXmlElement xs
  | otherwise = parseXmlString ys
