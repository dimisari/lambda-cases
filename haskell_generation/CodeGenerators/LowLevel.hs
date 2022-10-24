{-# LANGUAGE LambdaCase #-}

module CodeGenerators.LowLevel where

import Prelude ( String, (++), concat, map, error )

import Helpers ( (-->), (.>), parenthesis_comma_sep_g )
import Parsers.LowLevel
  ( Literal( Constant0, Constant1 ), NameExpression( Name )
  , TupleMatchingExpression( TupleMatching ), TypeExpression( Type )
  , TupleOrIntType( TupleType, IntType ), AtomicExpression( ConstantExp, NameExp ) )

{-
All:
Literal, NameExpression, TupleMatchingExpression, AtomicExpression, TypeExpression,
TupleOrIntType
-}

type HaskellSource = String

-- Literal

literal_g = ( \case
  Constant0 -> "0"
  Constant1 -> "1"
  ) :: Literal -> HaskellSource

-- NameExpression

name_expression_g = ( \(Name n) -> n
  ) :: NameExpression -> HaskellSource

-- TupleMatchingExpression

tuple_matching_expression_g = ( \(TupleMatching nes) -> nes --> \case
  [] -> error "should not have less than 2 in tuple"
  [ _ ] -> tuple_matching_expression_g (TupleMatching [])
  _ -> parenthesis_comma_sep_g name_expression_g nes
  ) :: TupleMatchingExpression -> HaskellSource

-- AtomicExpression

atomic_expression_g = ( \case
  ConstantExp ce -> literal_g ce
  NameExp ne -> name_expression_g ne
  ) :: AtomicExpression -> HaskellSource

-- TypeExpression

type_expression_g = ( \(Type toits toit) -> 
  toits-->map (tuple_or_int_g .> (++ " -> "))-->concat-->(++ tuple_or_int_g toit)
  ) :: TypeExpression -> HaskellSource

-- TupleOrIntType

tuple_or_int_g = ( \case
  TupleType tes -> parenthesis_comma_sep_g type_expression_g tes
  IntType -> "Int"
  ) :: TupleOrIntType -> HaskellSource

