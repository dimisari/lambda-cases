{-# LANGUAGE LambdaCase #-}

module HaskellTypes.LowLevel where

import Prelude ( String, Show, (++), show, concat, map )
import Helpers ( (-->), (.>) )

data Literal = Constant0 | Constant1
  deriving Show

newtype ValueName = VN String
instance Show ValueName where show = \(VN n) -> "Name " ++ n

data LiteralOrValueName = Literal Literal | ValueName ValueName
instance Show LiteralOrValueName where
  show = \case
    Literal l -> show l
    ValueName vn -> show vn

data ApplicationDirection = LeftApplication | RightApplication 
  deriving Show

newtype TupleMatching = FieldNames [ ValueName ]
  deriving Show

data Abstraction = ValueNameAb ValueName | TupleMatching TupleMatching
instance Show Abstraction where
  show = \case
    ValueNameAb vn -> show vn
    TupleMatching tm -> show tm

newtype Abstractions = Abstractions [ Abstraction ]
instance Show Abstractions where
  show = \(Abstractions as) -> as-->map (show .> (++ " abstraction "))-->concat
