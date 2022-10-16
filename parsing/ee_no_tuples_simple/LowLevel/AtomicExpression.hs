{-# LANGUAGE LambdaCase #-}

module LowLevel.AtomicExpression where

import Prelude (Eq, show, Show, String, (<$>), pure, (*>), (++))
import Text.Parsec ((<|>), char)
import Text.Parsec.String (Parser)
import LowLevel.Helpers (lowers_underscore)

-- ConstantExpression

data ConstantExpression = Constant0 | Constant1 deriving ( Eq )
instance Show ConstantExpression where
  show = \case
    Constant0 -> "0."
    Constant1 -> "1."

[ zero_p, one_p, constant_p ] =
  [ char '0' *> pure Constant0, char '1' *> pure Constant1, zero_p <|> one_p ]
  :: [ Parser ConstantExpression ]

-- NameExpression

newtype NameExpression = NameExpression String deriving ( Eq )
instance Show NameExpression where show  = \(NameExpression n) -> n ++ "."

name_expression_p = NameExpression <$> lowers_underscore
  :: Parser NameExpression

-- AtomicExpression

data AtomicExpression = Constant ConstantExpression | Name NameExpression
  deriving (Eq, Show)

atomic_expression_p = (Constant <$> constant_p) <|> (Name <$> name_expression_p)
  :: Parser AtomicExpression
