{-# language LambdaCase #-}

module HaskellTypes.LowLevel where

import Data.List
  ( intercalate )
import Helpers
  ( (==>) )

-- All: Types, Show instances

-- Types:
-- Literal, ValueName, ManyAbstractions, Abstraction
type Literal = Integer

newtype ValueName =
  VN String deriving (Eq, Ord)

data ManyAbstractions =
  MA ValueName ValueName [ ValueName ]

data Abstraction =
  NameAbstraction ValueName | ManyAbstractions ManyAbstractions | UseFields

-- Show instances:
-- ValueName, ManyAbstractions, Abstraction
instance Show ValueName where
  show = \(VN n) -> n

instance Show ManyAbstractions where
  show = \(MA vn1 vn2 vns) ->
    "(" ++ map show (vn1 : vn2 : vns)==>intercalate ", " ++ ")"

instance Show Abstraction where
  show = \case
    NameAbstraction vn -> show vn
    ManyAbstractions tm -> show tm
    UseFields -> "use_fields"
