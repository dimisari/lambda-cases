module TypeExpression where

import Prelude (Eq, Show, pure, (<*), (*>), (<*>), (<$>))
import Text.Parsec ( (<|>), try, char, string, sepBy1)
import Text.Parsec.String (Parser)

-- TypeExpression

data TypeExpression = FinalType TupleOrIntType | FunctionType TupleOrIntType TypeExpression
  deriving (Eq, Show)

type_expression_p = try function_type_p <|> tuple_or_int_expr_p
  :: Parser TypeExpression

tuple_or_int_expr_p = FinalType <$> tuple_or_int_p
  :: Parser TypeExpression

function_type_p = FunctionType <$> tuple_or_int_p <*> (string " -> " *> type_expression_p)
  :: Parser TypeExpression

-- TupleOrIntType

data TupleOrIntType = TupleType [ TypeExpression ] | IntType
  deriving (Eq, Show)

tuple_or_int_p = tuple_type_p <|> int_type_p
  :: Parser TupleOrIntType

tuple_type_p = TupleType <$> (char '(' *> sepBy1 type_expression_p (string ", ") <* char ')')
  :: Parser TupleOrIntType

int_type_p = string "Int" *> pure IntType
  :: Parser TupleOrIntType
