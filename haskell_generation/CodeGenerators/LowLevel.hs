{-# LANGUAGE LambdaCase #-}

module CodeGenerators.LowLevel where

import Prelude ( String, (++), concat, map, error )

import Helpers ( Haskell, (-->), (.>), parenthesis_comma_sep_g )
import Parsers.LowLevel
  ( Literal( Constant0, Constant1 ), ValueName( VN )
  , LiteralOrValueName( Literal, ValueName ), TupleMatching( FieldNames )
  , Abstraction( ValueNameAb, TupleMatching )
  , Abstractions( Abstractions )
  , TupleParenOrIntType( TupleType, ParenthesisType, IntType )
  , ValueType( AbstractionTypesAndResultType ) )

{-
  All:
  Literal, ValueName, LiteralOrValueName, TupleMatching, Abstraction, Abstractions,
  TupleParenOrIntType, ValueType
-}

-- Literal

literal_g = ( \case
  Constant0 -> "0"
  Constant1 -> "1"
  ) :: Literal -> Haskell

-- ValueName

value_name_g = ( \(VN vn) -> vn
  ) :: ValueName -> Haskell

-- LiteralOrValueName

literal_or_value_name_g = ( \case
  Literal l -> literal_g l
  ValueName vn -> value_name_g vn
  ) :: LiteralOrValueName -> Haskell

-- TupleMatching

tuple_matching_g = ( \(FieldNames vns) -> vns --> \case
  [] -> error "should not have less than 2 in tuple"
  [ _ ] -> tuple_matching_g (FieldNames [])
  _ -> parenthesis_comma_sep_g value_name_g vns
  ) :: TupleMatching -> Haskell

-- Abstraction

abstraction_g = ( \case
  ValueNameAb vn -> value_name_g vn
  TupleMatching tm -> tuple_matching_g tm
  ) :: Abstraction -> Haskell

-- Abstractions

abstractions_g = ( \(Abstractions as) ->
  as-->map (\a -> "\\" ++ abstraction_g a ++ " -> ")-->concat
  ) :: Abstractions -> Haskell

-- TupleParenOrIntType

tuple_paren_or_int_type_g = ( \case
  TupleType vts -> parenthesis_comma_sep_g value_type_g vts
  ParenthesisType vt -> case vt of
    (AbstractionTypesAndResultType [] tpoit) -> tuple_paren_or_int_type_g tpoit
    _ -> "(" ++ value_type_g vt ++ ")"
  IntType -> "Int"
  ) :: TupleParenOrIntType -> Haskell

-- ValueType

value_type_g = ( \(AbstractionTypesAndResultType tpoits tpoit) -> 
  tpoits-->map (tuple_paren_or_int_type_g .> (++ " -> "))-->concat
  ++ tuple_paren_or_int_type_g tpoit
  ) :: ValueType -> Haskell
