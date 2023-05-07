module Helpers where

import Text.Parsec
import Text.Parsec.Pos (newPos)
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

-- Haskell generation 

type Haskell = String

indent = ( \i -> replicate (2 * i) ' ' )
  :: Int -> Haskell

-- other

data Pos a =
  WithPos { get_pos :: SourcePos, remove_pos :: a }

instance Show a => Show (Pos a) where
  show = \(WithPos _ a) -> show a

add_dummy_pos = WithPos (newPos "" 0 0)
  :: a -> Pos a

add_pos_p = (<*>) (WithPos <$> getPosition)
  :: Parser a -> Parser (Pos a)
