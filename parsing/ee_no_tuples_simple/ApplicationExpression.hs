module ApplicationExpression where

import Prelude (Eq, Show, undefined, (<$>), (<*), (*>), (<*>))
import Text.Parsec ((<|>), try, char, sepBy1, string)
import Text.Parsec.String (Parser)
import AtomicExpression (AtomicExpression, atomic_expression_p)
import ValueExpression (ValueExpression, value_expression_p)

-- ParenthesisExpression

data ParenthesisExpression = ForPrecedence ValueExpression | Tuple [ ValueExpression ]
  deriving (Eq, Show)

parenthesis_expression_p =
  char '(' *> (try for_precedence_p <|> tuple_expression_p) <* char ')'
  :: Parser ParenthesisExpression

for_precedence_p = ForPrecedence <$> value_expression_p
  :: Parser ParenthesisExpression

tuple_expression_p = Tuple <$> sepBy1 value_expression_p (string ", ")
  :: Parser ParenthesisExpression

-- HighPrecedenceExpression

data HighPrecedenceExpression = Parenthesis ParenthesisExpression | Atomic AtomicExpression
  deriving (Eq, Show)

high_precedence_expression_p =
  (Parenthesis <$> parenthesis_expression_p) <|> (Atomic <$> atomic_expression_p)
  :: Parser HighPrecedenceExpression

-- ApplicationExpression

data ApplicationExpression = 
  LeftApplication HighPrecedenceExpression ApplicationExpression |
  RightApplication HighPrecedenceExpression ApplicationExpression |
  HighPrecedence HighPrecedenceExpression
  deriving (Eq, Show)

application_expression_p =
  try left_application_expression_p <|> try right_application_expression_p <|>
  (HighPrecedence <$> high_precedence_expression_p)
  :: Parser ApplicationExpression

left_application_expression_p =
  LeftApplication
    <$> (high_precedence_expression_p <* string "<--") <*> application_expression_p
    :: Parser ApplicationExpression

right_application_expression_p = 
  RightApplication
    <$> (high_precedence_expression_p <* string "-->") <*> application_expression_p
  :: Parser ApplicationExpression
