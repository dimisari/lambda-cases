module Parsers.LowLevel where

import Text.Parsec
import Text.Parsec.String (Parser)

import Helpers ((==>), keywords)

import HaskellTypes.LowLevel

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
  lower_under_p >>= \starting_char ->
  many (lower_under_p <|> digit) >>= \lowers_unders_or_digits ->
  let value_name = (starting_char : lowers_unders_or_digits) in
  elem value_name keywords ==> \case
    True -> parserFail "keyword"
    _ -> return $ VN value_name
  :: Parser ValueName 

lower_under_p = lower <|> char '_'
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
