module LowLevel.Helpers where

import Prelude ( String, Either, flip, return, (*>), (.), ($) )
import Text.Parsec ( (<|>), char, lower, many1, ParseError, parse, string )
import Text.Parsec.String ( Parser )

-- Helpers

lowers_underscore = many1 ( lower <|> char '_' )
  :: Parser String

parse_with = flip parse "" 
  :: Parser a -> String -> Either ParseError a

seperated2 = (\p -> \s -> do
  a <- p
  as <- many1 (string s *> p)
  return (a:as)
  )
  :: Parser a -> String -> Parser [a]

(.>) = flip (.)
  :: (a -> b) -> (b -> c) -> (a -> c)

(-->) = flip ($)
  :: a -> (a -> b) -> b
