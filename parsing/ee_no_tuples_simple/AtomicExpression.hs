module AtomicExpression where

import Prelude (Eq, Show, String, (<$>), pure, (*>))
import Text.Parsec ((<|>), char)
import Text.Parsec.String (Parser)
import Helpers (letters_underscore)

-- ConstantExpression

data ConstantExpression = Constant0 | Constant1
  deriving (Eq, Show)

constant_p = zero_p <|> one_p
  :: Parser ConstantExpression

zero_p = char '0' *> pure Constant0
  :: Parser ConstantExpression

one_p = char '1' *> pure Constant1
  :: Parser ConstantExpression

-- NameExpression

newtype NameExpression = NameExpression String
  deriving (Eq, Show)

name_expression_p = NameExpression <$> letters_underscore
  :: Parser NameExpression

-- AtomicExpression

data AtomicExpression = Constant ConstantExpression | Name NameExpression
  deriving (Eq, Show)

atomic_expression_p = (Constant <$> constant_p) <|> (Name <$> name_expression_p)
  :: Parser AtomicExpression
