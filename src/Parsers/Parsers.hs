module Parsers (
    Convertable,
    toJson,
    toXml
  ) where


import Data.Map.Strict (empty, fromList, toList)
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
_jsonArrayToXml xs = acc
  where
    f :: (Int, [XmlData]) -> JsonData -> (Int, [XmlData])
    f (c, acc) (JsonNull) = (c + 1, acc)
    f (c, acc) x = (c + 1, element : acc)
      where
        element = XmlElement {
          tag = "element",
          attributes = fromList [("index", show c)],
          children = [toXml x]
        }
    (_, acc) = foldl f (0, []) xs

_jsonObjectToXml :: [(String, JsonData)] -> [XmlData]
_jsonObjectToXml = foldl f []
    where
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
      tag = "array",
      attributes = empty,
      children = _jsonArrayToXml xs
    }
  toXml (JsonObject xs) =
    XmlElement {
      tag = "object",
      attributes = empty,
      children = _jsonObjectToXml $ toList xs
    }
