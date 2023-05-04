module Parsers.LowLevel where

import Text.Parsec
import Text.Parsec.String (Parser)

import Helpers ((==>), keywords, space_or_spicy_nl)

import ParsingTypes.LowLevel

-- All: Literal, ValueName, Abstraction, ManyAbstractions, Input

-- ValueName: value_name_p, lower_or_under_p

value_name_p =
  lower_or_under_p >>= \starting_char ->
  many (lower_or_under_p <|> digit) >>= \other_chars ->
  return $ VN (starting_char : other_chars)
  :: Parser ValueName 

lower_or_under_p = lower <|> char '_'
  :: Parser Char

-- PosValueName: pos_value_name_p

pos_value_name_p =
  PVN <$> getPosition <*> value_name_p
  :: Parser PosValueName 

-- Literal: literal_p, integer_p, char_p, string_p

literal_p =
  Integer <$> integer_p <|> String <$> string_p <|> Char <$> char_p
  :: Parser Literal

integer_p =
  read <$> ((:) <$> char '-' <*> many1 digit <|> many1 digit)
  :: Parser Integer

char_p = 
  char '\'' *> noneOf ['\''] <* char '\''
  :: Parser Char

string_p = char '"' *> many (noneOf ['"']) <* char '"'
  :: Parser String

-- PosLiteral: 

pos_literal_p =
  PL <$> getPosition <*> literal_p
  :: Parser PosLiteral

-- Abstraction: abstraction_p

abstraction_p =
  try (string "use_fields") *> return UseFields <|>
  AbstractionName <$> pos_value_name_p
  :: Parser Abstraction

-- PosAbstraction: pos_abstraction_p

pos_abstraction_p =
  PA <$> getPosition <*> abstraction_p
  :: Parser PosAbstraction

-- ManyAbstractions: many_abstractions_p

many_abstractions_p =
  string "(" >> pos_abstraction_p >>= \abstraction1 ->
  string ", " >> pos_abstraction_p >>= \abstraction2 ->
  many (string ", " >> pos_abstraction_p) >>= \abstractions ->
  string ")" >> return (Abstractions abstraction1 abstraction2 abstractions)
  :: Parser ManyAbstractions

-- PosManyAbstractions: pos_many_abstractions_p

pos_many_abstractions_p =
  PMA <$> getPosition <*> many_abstractions_p
  :: Parser PosManyAbstractions

-- Input: input_p

input_p =
  ( OneAbstraction <$> pos_abstraction_p <|>
    ManyAbstractions <$> pos_many_abstractions_p
  ) <* (string " ->" >> space_or_spicy_nl)
  :: Parser Input

-- PosInput: pos_input_p

pos_input_p =
  PI <$> getPosition <*> input_p
  :: Parser PosInput
