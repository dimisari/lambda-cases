module Parsing.Parsers.Helpers where

import Helpers 

import Text.Parsec
import Text.Parsec.String (Parser)

spaces_and_tabs = many $ char ' ' <|> char '\t'
  :: Parser String

spicy_new_line = spaces_and_tabs *> char '\n' <* spaces_and_tabs
  :: Parser Char

space_or_spicy_nl = char ' ' <|> spicy_new_line
  :: Parser Char

spicy_new_lines = skipMany1 spicy_new_line
  :: Parser ()

eof_or_spicy_nls = spicy_new_lines <|> eof
  :: Parser ()

add_pos_p = (<*>) (WithPos <$> getPosition)
  :: Parser a -> Parser (Pos a)
