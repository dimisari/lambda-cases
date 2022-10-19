{-# LANGUAGE LambdaCase #-}

module LowLevel.TypeExpression where

import Prelude ( Eq, show, Show, pure, (<*), (*>), (<*>), (<$>), (++) )
import Text.Parsec ( (<|>), try,  string, sepBy1 )
import Text.Parsec.String ( Parser )
import LowLevel.Helpers ( seperated2 )

-- TypeExpression

data TypeExpression =
  FunctionType TupleOrIntType TypeExpression | ResultType TupleOrIntType
  deriving (Eq)

instance Show TypeExpression where
  show = \case
    ResultType rt -> show rt
    FunctionType toit te -> show toit ++ " right_arrow " ++ show te

[ type_expression_p, function_type_p, result_p ] =
  [ try function_type_p <|> result_p
  , FunctionType <$> tuple_or_int_p <*> (string " -> " *> type_expression_p)
  , ResultType <$> tuple_or_int_p ]
  :: [ Parser TypeExpression ]

-- TupleOrIntType

data TupleOrIntType = TupleType [ TypeExpression ] | IntType
  deriving (Eq, Show)

[ tuple_or_int_p, tuple_type_p, int_type_p ] =
  [ tuple_type_p <|> int_type_p
  , TupleType <$> (string "( " *> seperated2 type_expression_p ", " <* string " )")
  , string "Int" *> pure IntType ]
  :: [ Parser TupleOrIntType ]
