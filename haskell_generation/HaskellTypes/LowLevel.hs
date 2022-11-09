{-# LANGUAGE LambdaCase #-}

module HaskellTypes.LowLevel where

import Prelude ( String, Show, (++), show, concatMap )
import Helpers ( (-->), (.>) )

-- types
data Literal = Constant0 | Constant1 deriving Show

newtype ValueName = VN String

data LiteralOrValueName = Literal Literal | ValueName ValueName

data ApplicationDirection = LeftApplication | RightApplication deriving Show

newtype TupleMatching = FieldNames [ ValueName ] deriving Show

data Abstraction = ValueNameAb ValueName | TupleMatching TupleMatching

newtype Abstractions = Abstractions [ Abstraction ]

-- Show instances
instance Show ValueName where show = \(VN n) -> "Name " ++ n

instance Show LiteralOrValueName where
  show = \case
    Literal l -> show l
    ValueName vn -> show vn

instance Show Abstraction where
  show = \case
    ValueNameAb vn -> show vn
    TupleMatching tm -> show tm

instance Show Abstractions where
  show = \(Abstractions as) -> as-->concatMap (show .> (++ " abstraction "))
