module Helpers where

import Text.Parsec ((<|>), many, many1, string, char, try, eof, skipMany1, digit)
import Text.Parsec.String (Parser)
import Data.List (intercalate)
import Control.Monad.State (State)
import Control.Monad.Trans.Except (catchE, throwE, ExceptT)

-- All:
-- Keywords, Function application/composition, Parsing, Haskell generation,
-- Debugging

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

spaces_tabs = many $ char ' ' <|> char '\t'
  :: Parser String

new_line_space_surrounded = spaces_tabs *> char '\n' <* spaces_tabs
  :: Parser Char

space_or_newline = char ' ' <|> try new_line_space_surrounded
  :: Parser Char

eof_or_new_lines = eof <|> skipMany1 new_line_space_surrounded
  :: Parser ()

-- Haskell generation 

type Error = String
type Haskell = String

indent = ( \i -> replicate (2 * i) ' ' )
  :: Int -> Haskell

-- Debugging: debug, debug_with_msg

debug = ( \f -> catchE f (\_ -> throwE "hi" ) )
  :: ExceptT String (State b) c -> ExceptT String (State b) c

debug_with_msg = ( \f s -> catchE f (\_ -> throwE s ) )
  :: ExceptT String (State b) c -> String -> ExceptT String (State b) c

