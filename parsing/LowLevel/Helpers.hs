module LowLevel.Helpers where

import Prelude ( String, Either, flip, return, (>>=), (*>), (.), ($) )
import Text.Parsec ( (<|>), char, lower, many1, ParseError, parse, string, try )
import Text.Parsec.String ( Parser )

-- Helpers

lowers_underscore = many1 ( lower <|> char '_' )
  :: Parser String

seperated2 = (\p -> \s ->
  p >>= \a ->
  try (string s *> p) --> many1 >>= \as ->
  return (a:as)
  )
  :: Parser a -> String -> Parser [a]

(.>) = flip (.)
  :: (a -> b) -> (b -> c) -> (a -> c)

(-->) = flip ($)
  :: a -> (a -> b) -> b
