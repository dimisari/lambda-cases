module Parsing.Types.LowLevel where

import Data.List (intercalate)
import Helpers

-- All (Types and Show instances):
-- ValueName, Literal, Abstraction, ManyAbstractions, Input

-- ValueName

newtype ValueName =
  VN String deriving (Eq, Ord)

instance Show ValueName where
  show = \(VN val_name_str) -> val_name_str

-- Literal

data Literal =
  Int Int | Char Char | String String

instance Show Literal where
  show = \case
    Int int -> show int
    Char char -> show char
    String string -> show string

-- Abstraction

data Abstraction =
  AbstractionName ValueName | UseFields

instance Show Abstraction where
  show = \case
    AbstractionName value_name -> show value_name
    UseFields -> "use_fields"

-- ManyAbstractions

data ManyAbstractions =
  Abstractions (Pos Abstraction) (Pos Abstraction) [ Pos Abstraction ]

instance Show ManyAbstractions where
  show = \(Abstractions abs1 abs2 rest_of_abs) ->
    "(" ++ map show (abs1 : abs2 : rest_of_abs)==>intercalate ", " ++ ")"

-- Input

data Input =
  OneAbstraction (Pos Abstraction) | ManyAbstractions ManyAbstractions

instance Show Input where
  show = show_input .> (++ " -> ") where
    show_input = \case
      OneAbstraction abstraction -> show abstraction
      ManyAbstractions abstractions -> show abstractions
