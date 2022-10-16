module Helpers where

import Prelude (String, Either, flip)
import Text.Parsec ((<|>), char, lower, many1, ParseError, parse)
import Text.Parsec.String (Parser)

-- Helpers

lowers_underscore = many1 (lower <|> char '_')
  :: Parser String

parse_with = flip parse "" 
  :: Parser a -> String -> Either ParseError a
