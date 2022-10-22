{-# LANGUAGE LambdaCase #-}

module CodeGenerators.LowLevelExpressions where

import Prelude
  ( String, (++), concat, map, init, last, error )

import Parsers.LowLevel.Expressions
  ( Literal( Constant0, Constant1 ) 
  , NameExpression( Name )
  , TupleMatchingExpression( TupleMatching ) 
  , TypeExpression( Type )
  , TupleOrIntType( TupleType, IntType )
  , AtomicExpression( ConstantExp, NameExp ) )
import Parsers.LowLevel.Helpers ( (-->), (.>) )

type HaskellSource = String

{-
All:
Literal, NameExpression, TupleMatchingExpression, AtomicExpression, TypeExpression,
TupleOrIntType
-}

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
  _ ->
    let
    all_but_last = init nes-->map (name_expression_g .> (++ ", "))-->concat
      :: String
    last_one = nes-->last-->name_expression_g
      :: String
    in
    "( " ++ all_but_last ++ last_one ++ " )"
  ) :: TupleMatchingExpression -> HaskellSource

-- AtomicExpression

atomic_expression_g = ( \case
  ConstantExp e -> literal_g e
  NameExp e -> name_expression_g e
  ) :: AtomicExpression -> HaskellSource

-- TypeExpression

type_expression_g = ( \(Type toits toit) -> 
  toits-->map (tuple_or_int_g .> (++ " -> "))-->concat-->(++ tuple_or_int_g toit)
  ) :: TypeExpression -> HaskellSource

-- TupleOrIntType

tuple_or_int_g = ( \case
  TupleType tes ->
    let
    all_but_last = init tes-->map (type_expression_g .> (++ ", "))-->concat
      :: String
    last_one = tes-->last-->type_expression_g
      :: String
    in
    "( " ++ all_but_last ++ last_one ++ " )"
  IntType -> "Int"
  ) :: TupleOrIntType -> HaskellSource

