module Parsers (
  Predicate,
  ParsingError,
  ParsingResult,
  ParsingResponse,
  Parser
  ) where

type Predicate a = a -> Bool

type ParsingError = (Int, String)
type ParsingResult a = (a, String)
type ParsingResponse a = Either ParsingError (ParsingResult a)
type Parser a = String -> ParsingResponse a
