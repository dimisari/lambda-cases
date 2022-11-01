{-# LANGUAGE LambdaCase #-}

module CodeGenerators.LowLevel where

import Prelude ( String, (++), concat, map, error )

import Helpers ( (-->), (.>), parenthesis_comma_sep_g )
import Parsers.LowLevel
  ( Literal( Constant0, Constant1 ), ValueName( VN )
  , LiteralOrValueName( Literal, ValueName ), TupleMatching( FieldNames )
  , AbstractionArgument( ValueNameAbstraction, TupleMatchingAbstraction )
  , AbstractionArguments( AbstractionArguments )
  , TupleParenOrIntType( TupleType, ParenthesisType, IntType )
  , ValueType( AbstractionTypesAndResultType )
  )

{-
  All:
  Literal, ValueName, LiteralOrValueName, TupleMatching,
  AbstractionArgument, AbstractionArguments,
  TupleParenOrIntType, ValueType
-}

type HaskellSource = String

-- Literal

literal_g = ( \case
  Constant0 -> "0"
  Constant1 -> "1"
  ) :: Literal -> HaskellSource

-- ValueName

value_name_g = ( \(VN n) -> n
  ) :: ValueName -> HaskellSource

-- LiteralOrValueName

literal_or_value_name_g = ( \case
  Literal ce -> literal_g ce
  ValueName ne -> value_name_g ne
  ) :: LiteralOrValueName -> HaskellSource

-- TupleMatching

tuple_matching_g = ( \(FieldNames nes) -> nes --> \case
  [] -> error "should not have less than 2 in tuple"
  [ _ ] -> tuple_matching_g (FieldNames [])
  _ -> parenthesis_comma_sep_g value_name_g nes
  ) :: TupleMatching -> HaskellSource

-- AbstractionArgument

abstraction_argument_g = ( \case
  ValueNameAbstraction e -> value_name_g e
  TupleMatchingAbstraction e -> tuple_matching_g e
  ) :: AbstractionArgument -> HaskellSource

-- AbstractionArguments

abstraction_arguments_g = ( \(AbstractionArguments aaes) ->
  aaes-->map (\x -> "\\" ++ abstraction_argument_g x ++ " -> ")-->concat
  ) :: AbstractionArguments -> HaskellSource

-- TupleParenOrIntType

tuple_paren_or_int_type_g = ( \case
  TupleType tes -> parenthesis_comma_sep_g value_type_g tes
  ParenthesisType te -> case te of
    (AbstractionTypesAndResultType [] hpte) -> tuple_paren_or_int_type_g hpte
    _ -> "(" ++ value_type_g te ++ ")"
  IntType -> "Int"
  ) :: TupleParenOrIntType -> HaskellSource

-- ValueType

value_type_g = ( \(AbstractionTypesAndResultType hptes hpte) -> 
  hptes-->map (tuple_paren_or_int_type_g .> (++ " -> "))-->concat
  ++ tuple_paren_or_int_type_g hpte
  ) :: ValueType -> HaskellSource

