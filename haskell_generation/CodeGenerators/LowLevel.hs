{-# LANGUAGE LambdaCase #-}

module CodeGenerators.LowLevel where

import Prelude ( String, (++), concat, map, error )

import Helpers ( (-->), (.>), parenthesis_comma_sep_g )
import Parsers.LowLevel
  ( Literal( Constant0, Constant1 ), ValueName( Name )
  , AtomicExpression( ConstantExp, NameExp ), TupleMatchingExpression( TupleMatching )
  , AbstractionArgumentExpression( NameExp_ab, TupleMatchingExp )
  , AbstractionArgumentsExpression( AbstractionArguments )
  , HighPrecedenceTypeExpression( TupleType, ForPrecedenceType, IntType )
  , TypeExpression( Type )
  )

{-
  All:
  Literal, ValueName, AtomicExpression, TupleMatchingExpression,
  AbstractionArgumentExpression, AbstractionArgumentsExpression,
  HighPrecedenceTypeExpression, TypeExpression
-}

type HaskellSource = String

-- Literal

literal_g = ( \case
  Constant0 -> "0"
  Constant1 -> "1"
  ) :: Literal -> HaskellSource

-- ValueName

name_expression_g = ( \(Name n) -> n
  ) :: ValueName -> HaskellSource

-- AtomicExpression

atomic_expression_g = ( \case
  ConstantExp ce -> literal_g ce
  NameExp ne -> name_expression_g ne
  ) :: AtomicExpression -> HaskellSource

-- TupleMatchingExpression

tuple_matching_expression_g = ( \(TupleMatching nes) -> nes --> \case
  [] -> error "should not have less than 2 in tuple"
  [ _ ] -> tuple_matching_expression_g (TupleMatching [])
  _ -> parenthesis_comma_sep_g name_expression_g nes
  ) :: TupleMatchingExpression -> HaskellSource

-- AbstractionArgumentExpression

abstraction_argument_g = ( \case
  NameExp_ab e -> name_expression_g e
  TupleMatchingExp e -> tuple_matching_expression_g e
  ) :: AbstractionArgumentExpression -> HaskellSource

-- AbstractionArgumentsExpression

abstraction_arguments_g = ( \(AbstractionArguments aaes) ->
  aaes-->map (\x -> "\\" ++ abstraction_argument_g x ++ " -> ")-->concat
  ) :: AbstractionArgumentsExpression -> HaskellSource

-- HighPrecedenceTypeExpression

high_precedence_type_expression_g = ( \case
  TupleType tes -> parenthesis_comma_sep_g type_expression_g tes
  ForPrecedenceType te -> case te of
    (Type [] hpte) -> high_precedence_type_expression_g hpte
    _ -> "(" ++ type_expression_g te ++ ")"
  IntType -> "Int"
  ) :: HighPrecedenceTypeExpression -> HaskellSource

-- TypeExpression

type_expression_g = ( \(Type hptes hpte) -> 
  hptes-->map (high_precedence_type_expression_g .> (++ " -> "))-->concat
  ++ high_precedence_type_expression_g hpte
  ) :: TypeExpression -> HaskellSource

