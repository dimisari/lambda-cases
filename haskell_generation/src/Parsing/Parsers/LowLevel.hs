module Parsing.Parsers.LowLevel where

import Text.Parsec
import Text.Parsec.String (Parser)

import Parsing.Parsers.Helpers

import Parsing.Types.LowLevel

-- All: Literal, ValueName, Abstraction, ManyAbstractions, Input

-- ValueName: value_name_p, lower_or_under_p

value_name_p =
  lower_or_under_p >>= \starting_char ->
  many (lower_or_under_p <|> digit) >>= \other_chars ->
  return $ VN (starting_char : other_chars)
  :: Parser ValueName 

lower_or_under_p = lower <|> char '_'
  :: Parser Char

-- Literal: literal_p, int_p, char_p, string_p

literal_p =
  Int <$> int_p <|> String <$> string_p <|> Char <$> char_p
  :: Parser Literal

int_p =
  read <$> ((:) <$> char '-' <*> many1 digit <|> many1 digit)
  :: Parser Int

char_p = 
  char '\'' *> noneOf ['\''] <* char '\''
  :: Parser Char

string_p = char '"' *> many (noneOf ['"']) <* char '"'
  :: Parser String

-- Abstraction: abstraction_p

abstraction_p =
  return UseFields <* try (string "use_fields") <|>
  AbstractionName <$> value_name_p
  :: Parser Abstraction

-- ManyAbstractions: many_abstractions_p

many_abstractions_p =
  string "(" >> add_pos_p abstraction_p >>= \abstraction1 ->
  string ", " >> add_pos_p abstraction_p >>= \abstraction2 ->
  many (string ", " >> add_pos_p abstraction_p) >>= \abstractions ->
  string ")" >> return (Abstractions abstraction1 abstraction2 abstractions)
  :: Parser ManyAbstractions

-- Input: input_p

input_p =
  ( OneAbstraction <$> add_pos_p abstraction_p <|>
    ManyAbstractions <$> many_abstractions_p
  ) <* (string " ->" >> space_or_spicy_nl)
  :: Parser Input

