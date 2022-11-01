module Helpers where

import Prelude
  ( String, (>>=), (<*), (*>), (.), ($), (++), flip, return, init, last, concat, map )
import Text.Parsec ( (<|>), many, many1, string, char, try )
import Text.Parsec.String ( Parser )

-- parsing 

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

new_line_space_surrounded = spaces_tabs *> char '\n' *> spaces_tabs
  :: Parser String

-- function application/composition

(-->) = flip ($)
  :: a -> (a -> b) -> b

(.>) = flip (.)
  :: (a -> b) -> (b -> c) -> (a -> c)

-- haskell generation 

type HaskellSource = String 

parenthesis_comma_sep_g = ( \g -> \l ->
  "( " ++ init l-->map (g .> (++ ", "))-->concat ++ l-->last-->g ++ " )"
  ) :: (a -> HaskellSource) -> [ a ] -> HaskellSource
