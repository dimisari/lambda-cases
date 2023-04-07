module Parsers.LowLevel where

import Text.Parsec
import Text.Parsec.String (Parser)

import Helpers ((==>), keywords)

import ParsingTypes.LowLevel

-- All: Literal, ValueName, Abstraction, ManyAbstractions, Input

-- Literal: literal_p, integer_p

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

-- ValueName: value_name_p

value_name_p =
  lower_case_or_underscore_p >>= \starting_char ->
  many (lower_case_or_underscore_p <|> digit) >>= \other_chars ->
  let value_name = (starting_char : other_chars) in
  elem value_name keywords ==> \case
    True -> parserFail "keyword"
    _ -> return $ VN value_name
  :: Parser ValueName 

lower_case_or_underscore_p = lower <|> char '_'
  :: Parser Char

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
