{-# LANGUAGE LambdaCase #-}

module LowLevel.TypeExpression where

import Prelude ( Eq, show, Show, pure, (<*), (*>), (<*>), (<$>), (++) )
import Text.Parsec ( (<|>), try,  string, sepBy1)
import Text.Parsec.String (Parser)

-- TypeExpression

data TypeExpression =
  Result TupleOrIntType | Function TupleOrIntType TypeExpression
  deriving (Eq)

instance Show TypeExpression where
  show = \case
    Result rt -> show rt
    Function toit te -> show toit ++ " right_arrow " ++ show te

[ type_expression_p, result_p, function_type_p ] =
  [ try function_type_p <|> result_p
  , Result <$> tuple_or_int_p
  , Function <$> tuple_or_int_p <*> (string " -> " *> type_expression_p) ]
  :: [ Parser TypeExpression ]

-- TupleOrIntType

data TupleOrIntType = TupleType [ TypeExpression ] | IntType
  deriving (Eq, Show)

[ tuple_or_int_p, tuple_type_p, int_type_p ] =
  [ tuple_type_p <|> int_type_p
  , TupleType <$> (string "( " *> sepBy1 type_expression_p (string ", ") <* string " )")
  , string "Int" *> pure IntType ]
  :: [ Parser TupleOrIntType ]
