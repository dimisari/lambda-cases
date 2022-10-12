module ValueExpression where

import Prelude (Eq, Show)
import Text.Parsec ()
import Text.Parsec.String (Parser)
import AtomicExpression (AtomicExpression)
import TypeExpression (TypeExpression)
import ApplicationExpression (ApplicationExpression, application_expression_p)

-- MultiplicationExpression

data MultiplicationExpression =
  Multiplication ApplicationExpression MultiplicationExpression |
  Application ApplicationExpression
  deriving (Eq, Show)

multiplication_expression_p = try multiplication_p <|> application_expression_p

multiplication_p =
  Multiplication
    <$> (application_expression_p <* string " * ") <*> multiplication_expression_p
  :: Parser MultiplicationExpression

-- SubtractionExpression

data SubtractionExpression =
  Subtraction MultiplicationExpression MultiplicationExpression |
  MultiplicationExp MultiplicationExpression
  deriving (Eq, Show)

subtraction_expression_p =
  try subtraction_p <|> (MultiplicationExp <$> multiplication_expression_p)

subtraction_p =
  Subtraction
    <$> (multiplication_expression_p <* string " * ") <*> multiplication_expression_p
  :: Parser SubtractionExpression
