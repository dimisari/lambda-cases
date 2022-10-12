module ValueExpression where

import Prelude (Eq, Show, String, undefined, (<$>), (<*), (*>))
import Text.Parsec (string, sepBy1, char, (<|>), try)
import Text.Parsec.String (Parser)
import TypeExpression (TypeExpression)

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

-- ValueExpression

data ValueExpression =
  Abstraction String ValueExpression |
  Case [ ( ValueExpression, ValueExpression ) ] |
  InteremediatesOutput [ NameTypeAndValue ]
  deriving (Eq, Show)

value_expression_p = undefined
  :: Parser ValueExpression

-- NameTypeAndValue

data NameTypeAndValue = NTAV
  { get_name :: String, get_type :: TypeExpression, get_value :: ValueExpression }
  deriving (Eq, Show)

--NameTypeAndValueLists

data NameTypeAndValueLists = NTAVL
  { get_names :: [ String ], get_types :: [ TypeExpression ]
  , get_values :: [ ValueExpression ] }
  deriving (Eq, Show)

-- Probably going to need 

to_NTAV_list = undefined
  :: NameTypeAndValueLists -> [ NameTypeAndValue ]
