{-# language LambdaCase #-}

module CodeGenerators.LowLevel where

import Prelude ( Bool(..), (++), (==), concatMap, error, zip, init, last, map, length )

import Helpers ( Haskell, (-->) )
import HaskellTypes.LowLevel
  ( Literal(..), ValueName(..), LiteralOrValueName(..), TupleMatching(..)
  , Abstraction(..), Abstractions(..) )
import HaskellTypes.Types ( BaseType(..), ValueType(..) )

literal_g = ( \_ -> \case
  Constant0 -> "0"
  Constant1 -> "1"
  ) :: ValueType -> Literal -> Haskell

value_name_g = ( \_ -> \(VN vn) -> vn)
  :: ValueType -> ValueName -> Haskell

literal_or_value_name_g = ( \vt -> \case
  Literal l -> literal_g vt l
  ValueName vn -> value_name_g vt vn
  ) :: ValueType -> LiteralOrValueName -> Haskell

tuple_matching_g = ( \case
  TupleType vts ->
    \(FieldNames vns) -> vns --> \case
      [] -> error "should not have less than 2 in tuple"
      [ _ ] -> error "should not have less than 2 in tuple"
      _ -> case length vts == length vns of
        True ->
          zip vts vns-->map ( \( vt, vn ) -> value_name_g vt vn)--> \vns_g ->
          "( " ++ init vns_g-->concatMap (++ ", ") ++ last vns_g ++ " )"
        _ -> error "tuple values and tuple types must be of the same length"
  ParenthesisType (AbstractionTypesAndResultType [] bt) -> tuple_matching_g bt
  _ -> error "tuple matching a TypeName" -- possibly later with symbol table ?
  ) :: BaseType -> TupleMatching -> Haskell

abstraction_g = ( \bt -> \case
  ValueNameAb vn -> value_name_g (AbstractionTypesAndResultType [] bt) vn
  TupleMatching tm -> tuple_matching_g bt tm
  ) :: BaseType -> Abstraction -> Haskell

abstractions_g = ( \bts -> \(As as) ->
  zip bts as-->concatMap ( \(bt, a) -> "\\" ++ abstraction_g bt a ++ " -> ")
  ) :: [ BaseType ] -> Abstractions -> Haskell
