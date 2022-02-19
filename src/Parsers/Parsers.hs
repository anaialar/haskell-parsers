module Parsers (
    Convertable,
    toJson,
    toXml
  ) where

import Data.Map.Strict (Map, empty, fromList, toList, insert, insertWith, union, size, delete)
import qualified Data.Map.Strict as Map (null, map, lookup)
import Parsers.Json (
  JsonData (JsonNull, JsonNumber, JsonString, JsonBool, JsonArray, JsonObject),
  stringifyJson
  )
import Parsers.Xml (
  XmlData (XmlElement, tag, attributes, children, XmlString)
  )

class Convertable a where
  toJson :: a -> JsonData
  toXml :: a -> XmlData

_jsonArrayToXml :: [JsonData] -> [XmlData]
_jsonArrayToXml = snd . (foldr f (0, []))
  where
    f :: JsonData -> (Int, [XmlData]) -> (Int, [XmlData])
    f JsonNull (c, acc) = (c + 1, acc)
    f x (c, acc) = (c + 1, element : acc)
      where
        element = XmlElement {
          tag = "element",
          attributes = fromList [("index", show c)],
          children = [toXml x]
        }

_jsonObjectToXml :: [(String, JsonData)] -> [XmlData]
_jsonObjectToXml = foldl f []
    where
      f :: [XmlData] -> (String, JsonData) -> [XmlData]
      f acc (_, JsonNull) = acc
      f acc (k, JsonArray xs) = element : acc
        where
          element = XmlElement {
            tag = k,
            attributes = empty,
            children = _jsonArrayToXml xs
          }
      f acc (k, JsonObject xs) = element : acc
        where
          element = XmlElement {
            tag = k,
            attributes = empty,
            children = _jsonObjectToXml $ toList xs
          }
      f acc (k, v) = element : acc
        where
          element = XmlElement {
            tag = k,
            attributes = empty,
            children = [toXml v]
          }

instance Convertable JsonData where
  toJson = id
  toXml (JsonNull) = XmlString ""
  toXml (JsonNumber x) = XmlString $ show x
  toXml x@(JsonBool _) = XmlString $ stringifyJson x
  toXml (JsonString xs) = XmlString xs
  toXml (JsonArray xs) =
    XmlElement {
      tag = "list",
      attributes = empty,
      children = _jsonArrayToXml xs
    }
  toXml (JsonObject xs) =
    XmlElement {
      tag = "root",
      attributes = empty,
      children = _jsonObjectToXml $ toList xs
    }

instance Convertable XmlData where
  toXml = id
  toJson (XmlString xs) = JsonString xs
  toJson (XmlElement "list" attributes children)
    | Map.null attributes = x
    | otherwise = JsonObject $ insert "value" x $ Map.map JsonString attributes
    where
      x = JsonArray $ map f children

      f :: XmlData -> JsonData
      f (XmlElement tag attributes children) = toJson $ XmlElement tag (delete "index" attributes) children
      f x = toJson x

  toJson (XmlElement "array" attributes children) = toJson $ XmlElement "list" attributes children

  toJson (XmlElement _ attributes children)
    | Map.null x && (size y == 1) =
      case Map.lookup "children" y of
        Just x -> x
        Nothing -> JsonObject y
    | otherwise = JsonObject $ union x y
    where
      x = Map.map JsonString attributes
      y = snd $ foldl f (0, empty) children :: Map String JsonData

      f :: (Int, Map String JsonData) -> XmlData -> (Int, Map String JsonData)
      f (c, acc) (XmlString xs) = (c + 1, x)
        where
          x = if c > 0
            then insertWith f' "children" (JsonArray [JsonString xs]) acc
            else insert "children" (JsonString xs) acc
      f (c, acc) e@(XmlElement tag attributes children) = (c, insert tag (toJson e) acc)

      f' :: JsonData -> JsonData -> JsonData
      f' xs@(JsonString _) (JsonArray ys) = JsonArray $ [xs] ++ ys
      f' (JsonArray xs) (JsonArray ys) = JsonArray $ xs ++ ys
