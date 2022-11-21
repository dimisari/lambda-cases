{-# language LambdaCase #-}

module HaskellTypes.LowLevel where

import Prelude ( String, Show, Eq, Ord, (++), show, concatMap )
import Helpers ( (-->), (.>) )

-- Types
data Literal =
  Constant0 | Constant1  deriving Show

newtype ValueName =
  VN String deriving ( Eq, Ord )

data LiteralOrValueName =
  Literal Literal | ValueName ValueName

data ApplicationDirection =
  LeftApplication | RightApplication deriving Show

newtype TupleMatching =
  TM [ ValueName ] deriving Show

data Abstraction =
  ValueNameAb ValueName | TupleMatching TupleMatching

newtype Abstractions =
  As [ Abstraction ]

-- Show instances
instance Show ValueName where
  show = \(VN n) -> "Name " ++ n

instance Show LiteralOrValueName where
  show = \case
    Literal l -> show l
    ValueName vn -> show vn

instance Show Abstraction where
  show = \case
    ValueNameAb vn -> show vn
    TupleMatching tm -> show tm

instance Show Abstractions where
  show = \(As as) -> as-->concatMap (show .> (++ " abstraction "))
