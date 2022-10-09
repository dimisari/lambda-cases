module Helpers where

import Prelude (String, Either, flip, (.), (*>), map)
import Text.Parsec ((<|>), space, char, letter, many1, ParseError, parse)
import Text.Parsec.String (Parser)

-- Helpers

[ letters, spaces1 ] = map many1 [ letter, space ]
  :: [ Parser String ]

spaces_then = ( spaces1 *> )
  :: Parser a -> Parser a

letters_underscore = many1 (letter <|> char '_')
  :: Parser String

(.>) = flip (.)
  :: (a -> b) -> (b -> c) -> a -> c

parse_with = flip parse "" 
  :: Parser a -> String -> Either ParseError a
