{-# LANGUAGE LambdaCase #-}

module CodeGenerators.LowLevel where

import Prelude ( (++), concatMap, error )

import Helpers ( Haskell, (-->), parenthesis_comma_sep_g )
import HaskellTypes.LowLevel
  ( Literal(..), ValueName(..), LiteralOrValueName(..), TupleMatching(..)
  , Abstraction(..), Abstractions(..) )

literal_g = ( \case
  Constant0 -> "0"
  Constant1 -> "1"
  ) :: Literal -> Haskell

value_name_g = ( \(VN vn) -> vn
  ) :: ValueName -> Haskell

literal_or_value_name_g = ( \case
  Literal l -> literal_g l
  ValueName vn -> value_name_g vn
  ) :: LiteralOrValueName -> Haskell

tuple_matching_g = ( \(FieldNames vns) -> vns --> \case
  [] -> error "should not have less than 2 in tuple"
  [ _ ] -> tuple_matching_g (FieldNames [])
  _ -> parenthesis_comma_sep_g value_name_g vns
  ) :: TupleMatching -> Haskell

abstraction_g = ( \case
  ValueNameAb vn -> value_name_g vn
  TupleMatching tm -> tuple_matching_g tm
  ) :: Abstraction -> Haskell

abstractions_g = ( \(Abstractions as) ->
  as-->concatMap (\a -> "\\" ++ abstraction_g a ++ " -> ")
  ) :: Abstractions -> Haskell
