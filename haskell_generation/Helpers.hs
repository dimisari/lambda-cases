module Helpers where

import Prelude
  ( String, Char, (>>=), (<*), (*>), (.), ($), (++), flip, return, init, last, concat
  , map )
import Text.Parsec ( (<|>), many, many1, string, char, try )
import Text.Parsec.String ( Parser )

{-
  All:
  Keywords, Function application/composition, Parsing, Haskell generation
-}

-- Keywords

keywords =
  [ "tuple_type", "value", "or_type", "values" , "use_tuple_fields", "cases"
  , "case_value", "intermediates", "output", "type_predicate", "function", "functions"
  , "type_theorem", "proof" ]
  :: [ String ]

-- Function application/composition

(-->) = flip ($)
  :: a -> (a -> b) -> b
(.>) = flip (.)
  :: (a -> b) -> (b -> c) -> (a -> c)

-- Parsing 

seperated2 = (\p -> \s ->
  p >>= \a ->
  try (string s *> p) --> many1 >>= \as ->
  return $ a:as
  ) :: Parser a -> String -> Parser [a]

comma_seperated2 = flip seperated2 ", "
  :: Parser a -> Parser [a]

paren_comma_seperated2 = ( \p -> string "( " *> comma_seperated2 p <* string " )" )
  :: Parser a -> Parser [a]

spaces_tabs = many $ char ' ' <|> char '\t'
  :: Parser String

new_line_space_surrounded = spaces_tabs *> char '\n' <* spaces_tabs
  :: Parser Char

-- Haskell generation 

type Haskell = String 

parenthesis_comma_sep_g = ( \g -> \l ->
  "( " ++ init l-->map (g .> (++ ", "))-->concat ++ l-->last-->g ++ " )"
  ) :: (a -> Haskell) -> [ a ] -> Haskell
