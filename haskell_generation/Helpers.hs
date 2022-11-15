module Helpers where

import Prelude
  ( Int, String, Char, (*), (>>=), (<*), (*>), (.), ($), (++), flip, return, init, last
  , concatMap )
import Text.Parsec ( (<|>), many, many1, string, char, try )
import Text.Parsec.String ( Parser )
import Data.List ( replicate )

{-
  All:
  Keywords, Function application/composition, Parsing, Haskell generation
-}

-- Keywords
keywords =
  [ "tuple_type", "value", "or_type", "values" , "use_fields", "cases"
  , "case_value", "intermediates", "output", "type_predicate", "function", "functions"
  , "type_theorem", "proof" ]
  :: [ String ]

-- Function application/composition
(-->) = flip ($)
  :: a -> (a -> b) -> b
(.>) = flip (.)
  :: (a -> b) -> (b -> c) -> (a -> c)

-- Parsing 
seperated2 = (\p s ->
  p >>= \a ->
  try (string s *> p) --> many1 >>= \as ->
  return $ a:as
  ) :: Parser a -> String -> Parser [a]

comma_seperated2 = flip seperated2 ", "
  :: Parser a -> Parser [a]

paren_comma_seperated2 = ( \p ->
  string "( " *> comma_seperated2 p <* string " )"
  ) :: Parser a -> Parser [a]

spaces_tabs = many $ char ' ' <|> char '\t'
  :: Parser String

new_line_space_surrounded = spaces_tabs *> char '\n' <* spaces_tabs
  :: Parser Char

-- Haskell generation 
type Haskell = String

indent = ( \i -> replicate (2 * i) ' ' )
  :: Int -> Haskell

parenthesis_comma_sep_g = ( \g -> \l ->
  "( " ++ init l-->concatMap (g .> (++ ", ")) ++ l-->last-->g ++ " )"
  ) :: (a -> Haskell) -> [ a ] -> Haskell
