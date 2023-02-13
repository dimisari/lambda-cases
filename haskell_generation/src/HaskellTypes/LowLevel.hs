{-# language LambdaCase #-}

module HaskellTypes.LowLevel where

import Data.List
  ( intercalate )
import Helpers
  ( (==>) )

-- All: Types, Show instances

-- Types:
-- Literal, ValueName, LiteralOrValueName, TupleMatching, Abstraction
type Literal = Integer

newtype ValueName =
  VN String deriving (Eq, Ord)

data LiteralOrValueName =
  Literal Literal | ValueName ValueName

data TupleMatching =
  TM ValueName ValueName [ ValueName ]

data Abstraction =
  ValueNameAb ValueName | TupleMatching TupleMatching | UseFields

-- Show instances:
-- ValueName, LiteralOrValueName, TupleMatching, Abstraction
instance Show ValueName where
  show = \(VN n) -> n

instance Show LiteralOrValueName where
  show = \case
    Literal l -> show l
    ValueName vn -> show vn

instance Show TupleMatching where
  show = \(TM vn1 vn2 vns) ->
    "(" ++ map show (vn1 : vn2 : vns)==>intercalate ", " ++ ")"

instance Show Abstraction where
  show = \case
    ValueNameAb vn -> show vn
    TupleMatching tm -> show tm
    UseFields -> "use_fields"
