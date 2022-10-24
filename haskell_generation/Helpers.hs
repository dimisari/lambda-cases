module Helpers where

import Prelude
  ( String, (>>=), (*>), (.), ($), (++), flip, return, init, last, concat, map )
import Text.Parsec ( (<|>), many, many1, string, char, try )
import Text.Parsec.String ( Parser )

seperated2 = (\p -> \s ->
  p >>= \a ->
  try (string s *> p) --> many1 >>= \as ->
  return (a:as)
  ) :: Parser a -> String -> Parser [a]

spaces_tabs = many $ char ' ' <|> char '\t'
  :: Parser String

(.>) = flip (.)
  :: (a -> b) -> (b -> c) -> (a -> c)

(-->) = flip ($)
  :: a -> (a -> b) -> b

type HaskellSource = String 

parenthesis_comma_sep_g = ( \g -> \l ->
  let
  all_but_last = init l-->map (g .> (++ ", "))-->concat
    :: HaskellSource
  last_one = l-->last-->g
    :: HaskellSource
  in
  "( " ++ all_but_last ++ last_one ++ " )"
  ) :: (a -> HaskellSource) -> [ a ] -> HaskellSource
