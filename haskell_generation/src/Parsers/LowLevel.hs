{-# language LambdaCase #-}

module Parsers.LowLevel where

import Text.Parsec
  ( (<|>), char, string, parserFail, many, many1, lower, try, digit )
import Text.Parsec.String
  ( Parser )

import Helpers
  ( (==>), keywords, seperated2 )

import HaskellTypes.LowLevel
  ( Literal(..), ValueName(..), ManyAbstractions(..), Abstraction(..), Input(..) )

-- All: Literal, ValueName, Abstraction, ManyAbstractions, Input

-- Literal: literal_p, integer_p

literal_p = integer_p
  :: Parser Literal

integer_p =
  let natural_number_string = many1 digit :: Parser String in
  read <$> ((:) <$> char '-' <*> natural_number_string <|> natural_number_string)
  :: Parser Integer

-- ValueName: value_name_p

value_name_p =
  many1 (lower <|> char '_') >>= \lowers_underscores ->
  elem lowers_underscores keywords ==> \case
    True -> parserFail "keyword"
    _ -> return $ VN lowers_underscores
  :: Parser ValueName 

-- Abstraction: abstraction_p

abstraction_p =
  string "use_fields" *> return UseFields <|> AbstractionName <$> value_name_p
  :: Parser Abstraction

-- ManyAbstractions: many_abstractions_p

many_abstractions_p =
  string "(" >> abstraction_p >>= \abstraction1 ->
  string ", " >> abstraction_p >>= \abstraction2 ->
  many (string ", " >> abstraction_p) >>= \abstractions ->
  string ")" >> return (Abstractions abstraction1 abstraction2 abstractions)
  :: Parser ManyAbstractions

-- Input: input_p

input_p =
  (OneAbstraction <$> abstraction_p <|> ManyAbstractions <$> many_abstractions_p)
  <* string " -> "
  :: Parser Input
