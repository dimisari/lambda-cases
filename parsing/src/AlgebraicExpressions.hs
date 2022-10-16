{-# LANGUAGE LambdaCase #-}

module AlgebraicExpressions where

import Prelude (Eq, Show, show, (<*>), (<*), (<$>), (++))
import Text.Parsec (try, string, (<|>))
import Text.Parsec.String (Parser)
import AtomicExpression (AtomicExpression)
import TypeExpression (TypeExpression)
import ApplicationExpression (ApplicationExpression, application_expression_p)

-- MultiplicationExpression

data MultiplicationExpression =
  Multiplication ApplicationExpression MultiplicationExpression |
  Application ApplicationExpression
  deriving (Eq)

instance Show MultiplicationExpression where
  show = \case
    Multiplication ae me -> "(" ++ show ae ++ " mul " ++ show me ++ ")"
    Application ae -> show ae

[ multiplication_p, multiplication_expression_p ] =
  [ Multiplication
    <$> (application_expression_p <* string " * ") <*> multiplication_expression_p
  , try multiplication_p <|> (Application <$> application_expression_p)
  ]
  :: [ Parser MultiplicationExpression ]

-- SubtractionExpression

data SubtractionExpression =
  Subtraction MultiplicationExpression MultiplicationExpression |
  MultiplicationExp MultiplicationExpression
  deriving (Eq)

instance Show SubtractionExpression where
  show = \case
    Subtraction me1 me2 -> "\t" ++ show me1 ++ " minus " ++ show me2
    MultiplicationExp me -> show me

subtraction_expression_p =
  try subtraction_p <|> (MultiplicationExp <$> multiplication_expression_p)
  :: Parser SubtractionExpression

subtraction_p =
  Subtraction
    <$> (multiplication_expression_p <* string " - ") <*> multiplication_expression_p
  :: Parser SubtractionExpression
