{-# LANGUAGE LambdaCase #-}

module ApplicationExpression where

import Prelude (Eq, Show, show, undefined, (<$>), (<*), (*>), (<*>), (++))
import Text.Parsec ((<|>), try, char, sepBy1, string)
import Text.Parsec.String (Parser)
import AtomicExpression (AtomicExpression, atomic_expression_p)
import ValueExpression
  (ValueExpression, value_expression_p, ParenthesisExpression, parenthesis_expression_p)

-- HighPrecedenceExpression

data HighPrecedenceExpression =
  Parenthesis ParenthesisExpression | Atomic AtomicExpression
  deriving (Eq)

instance Show HighPrecedenceExpression where
  show = \case
    Parenthesis pe -> show pe
    Atomic ae -> show ae

high_precedence_expression_p =
  (Parenthesis <$> parenthesis_expression_p) <|> (Atomic <$> atomic_expression_p)
  :: Parser HighPrecedenceExpression

-- ApplicationExpression

data ApplicationExpression = 
  LeftApplication HighPrecedenceExpression ApplicationExpression |
  RightApplication HighPrecedenceExpression ApplicationExpression |
  HighPrecedence HighPrecedenceExpression
  deriving (Eq)

instance Show ApplicationExpression where
  show = \case
    LeftApplication hpe ae -> show hpe ++ " left_application " ++ show ae
    RightApplication hpe ae -> show hpe ++ " right_application " ++ show ae
    HighPrecedence hpe -> show hpe

application_expression_p =
  try left_application_expression_p <|> try right_application_expression_p <|>
  (HighPrecedence <$> high_precedence_expression_p)
  :: Parser ApplicationExpression

[ left_application_expression_p, right_application_expression_p ] = 
  [ LeftApplication
    <$> (high_precedence_expression_p <* string "<--") <*> application_expression_p
  , RightApplication
    <$> (high_precedence_expression_p <* string "-->") <*> application_expression_p
  ]
  :: [ Parser ApplicationExpression ]
