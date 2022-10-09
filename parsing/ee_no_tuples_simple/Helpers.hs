module Helpers where

import Prelude (String, Either, flip)
import Text.Parsec ((<|>), char, letter, many1, ParseError, parse)
import Text.Parsec.String (Parser)

-- Helpers

letters_underscore = many1 (letter <|> char '_')
  :: Parser String

parse_with = flip parse "" 
  :: Parser a -> String -> Either ParseError a
