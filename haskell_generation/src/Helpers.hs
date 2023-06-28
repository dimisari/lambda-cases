module Helpers where

import Text.Parsec (SourcePos)
import Text.Parsec.Pos (newPos)

-- All:
-- Keywords, Function application/composition, Haskell generation, other

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

-- Haskell generation 

type Haskell = String

-- other

data Pos a =
  WithPos { get_pos :: SourcePos, remove_pos :: a }

instance Show a => Show (Pos a) where
  show = \(WithPos _ a) -> show a

add_dummy_pos = WithPos (newPos "" 0 0)
  :: a -> Pos a
