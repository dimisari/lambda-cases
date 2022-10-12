{-# LANGUAGE LambdaCase #-}

module AtomicExpression where

import Prelude (Eq, show, Show, String, (<$>), pure, (*>), (++))
import Text.Parsec ((<|>), char)
import Text.Parsec.String (Parser)
import Helpers (lowers_underscore)

-- ConstantExpression

data ConstantExpression = Constant0 | Constant1
  deriving (Eq)

instance Show ConstantExpression where
  show = \case
    Constant0 -> "0\n"
    Constant1 -> "1\n"

constant_p = zero_p <|> one_p
  :: Parser ConstantExpression

zero_p = char '0' *> pure Constant0
  :: Parser ConstantExpression

one_p = char '1' *> pure Constant1
  :: Parser ConstantExpression

-- NameExpression

newtype NameExpression = NameExpression String
  deriving (Eq)

instance Show NameExpression where
  show  = \(NameExpression n) -> n ++ "\n"

name_expression_p = NameExpression <$> lowers_underscore
  :: Parser NameExpression

-- AtomicExpression

data AtomicExpression = Constant ConstantExpression | Name NameExpression
  deriving (Eq, Show)

atomic_expression_p = (Constant <$> constant_p) <|> (Name <$> name_expression_p)
  :: Parser AtomicExpression
