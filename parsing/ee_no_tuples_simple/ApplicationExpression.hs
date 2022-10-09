module ApplicationExpression where

import Prelude (Eq, Show, undefined)
import Text.Parsec ((<|>), try)
import Text.Parsec.String (Parser)
import AtomicExpression (AtomicExpression)
import ValueExpression (ValueExpression)

-- ParenthesisExpression

data ParenthesisExpression = ForPrecedence ValueExpression | Tuple [ ValueExpression ]
  deriving (Eq, Show)

parenthesis_expression_p = try for_precedence_p <|> tuple_expression_p
  :: Parser ValueExpression

for_precedence_p = undefined
  :: Parser ValueExpression

tuple_expression_p = undefined
  :: Parser ValueExpression

-- HighPrecedenceExpression

data HighPrecedenceExpression = Parenthesis ParenthesisExpression | Atomic AtomicExpression
  deriving (Eq, Show)

high_precedence_expression_p = undefined
  :: Parser HighPrecedenceExpression

-- ApplicationExpression

data ApplicationExpression = 
  LeftApplication HighPrecedenceExpression ApplicationExpression |
  RightApplication HighPrecedenceExpression ApplicationExpression 
  deriving (Eq, Show)

application_expression_p = undefined
  :: Parser ApplicationExpression
