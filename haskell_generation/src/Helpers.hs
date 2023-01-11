module Helpers where

import Text.Parsec
  ( (<|>), many, many1, string, char, try, eof, skipMany1, digit )
import Text.Parsec.String
  ( Parser )
import Data.List
  ( intercalate )

-- All: Keywords, Function application/composition, Parsing, Haskell generation

-- Keywords
keywords =
  [ "tuple_type", "or_type", "values" , "use_fields", "cases"
  , "let", "output", "type_predicate", "function", "functions"
  , "type_theorem", "proof" ]
  :: [ String ]

-- Flipped function application/composition
(==>) = flip ($)
  :: a -> (a -> b) -> b
(.>) = flip (.)
  :: (a -> b) -> (b -> c) -> (a -> c)

-- Parsing 
seperated2 = (\s p ->
  p >>= \a -> try (string s *> p) ==> many1 >>= \as -> return $ a:as
  ) :: String -> Parser a -> Parser [a]

spaces_tabs = many $ char ' ' <|> char '\t'
  :: Parser String

new_line_space_surrounded = spaces_tabs *> char '\n' <* spaces_tabs
  :: Parser Char

space_or_newline = try new_line_space_surrounded <|> char ' '
  :: Parser Char

eof_or_new_lines = eof <|> skipMany1 new_line_space_surrounded
  :: Parser ()

-- Haskell generation 
type Haskell = String

indent = ( \i -> replicate (2 * i) ' ' )
  :: Int -> Haskell
