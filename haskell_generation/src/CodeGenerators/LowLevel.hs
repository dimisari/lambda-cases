{-# language LambdaCase #-}

module CodeGenerators.LowLevel where

import qualified Data.Map as M
  ( lookup )

import Helpers
  ( Haskell, (==>), (.>), paren_comma_sep_g )

import HaskellTypes.LowLevel
  ( Literal(..), ValueName(..), LiteralOrValueName(..), TupleMatching(..)
  , Abstraction(..), Abstractions(..) )
import HaskellTypes.Types
  ( ParenType(..), BaseType(..), ValueType(..), TypeName(..)
  , vt_shortest_equivalent )
import HaskellTypes.Generation
  ( Stateful, value_map_get, value_map_insert )

import CodeGenerators.ErrorMessages
  ( literal_not_int_err, type_check_err, tuple_matching_err
  , tuple_function_type_err, tuple_less_than_2_err
  , tuple_values_types_lengths_dont_match_err
  , abstractions_types_lengths_dont_match_err )

-- All:
-- Literal, LiteralOrValueName, TupleMatching, abstraction_g, Abstractions

-- Literal: literal_g, literal_type_inference_g
literal_g = ( vt_shortest_equivalent .> \case
  AbsTypesAndResType [] (TypeName (TN "Int")) -> show
  vt -> error $ literal_not_int_err vt
  ) :: ValueType -> Literal -> Haskell

literal_type_inference_g = ( \l -> 
  (AbsTypesAndResType [] (TypeName (TN "Int")), show l)
  ) :: Literal -> ( ValueType, Haskell )

-- LiteralOrValueName:
-- literal_or_value_name_g, type_check_value_name_g,
-- literal_or_value_name_type_inference_g
literal_or_value_name_g = ( \vt -> \case
  Literal l -> return $ literal_g vt l
  ValueName vn ->
    value_map_get vn >>= \lookup_vt -> type_check_value_name_g vt lookup_vt vn
  ) :: ValueType -> LiteralOrValueName -> Stateful Haskell

type_check_value_name_g = ( \vt lookup_vt vn -> case vt == lookup_vt of 
  False -> error $ type_check_err vn lookup_vt vt 
  True -> return $ show vn
  ) :: ValueType -> ValueType -> ValueName -> Stateful Haskell

literal_or_value_name_type_inference_g = ( \case
  Literal l -> return $ literal_type_inference_g l
  ValueName vn -> value_map_get vn >>= \vt -> return ( vt, show vn )
  ) :: LiteralOrValueName -> Stateful ( ValueType, Haskell )

-- TupleMatching:
-- tuple_matching_g, value_type_tuple_matching_g, value_types_tuple_matching_g,
-- correct_value_types_value_names_g
tuple_matching_g = ( \case
  -- possibly later with symbol table ?
  ParenType pt -> paren_tuple_matching_g pt
  TypeName tn -> undefined
  ) :: BaseType -> TupleMatching -> Stateful Haskell

paren_tuple_matching_g = ( \case
  -- possibly later with symbol table ?
  ParenVT vt -> value_type_tuple_matching_g vt
  TupleType vts -> value_types_tuple_matching_g vts 
  ) :: ParenType -> TupleMatching -> Stateful Haskell

value_type_tuple_matching_g = ( \case
  vt@(AbsTypesAndResType (_:_) _) -> error $ tuple_function_type_err vt
  -- maybe throw some warning or error here ? when could it arise ?
  AbsTypesAndResType [] bt -> tuple_matching_g bt
  ) :: ValueType -> TupleMatching -> Stateful Haskell

value_types_tuple_matching_g = ( \vts (TM vn1 vn2 vns) ->
  case length vts == length vns + 2 of
    False -> error tuple_values_types_lengths_dont_match_err
    True -> correct_value_types_value_names_g vts (vn1 : vn2 : vns)
  ) :: [ ValueType ] -> TupleMatching -> Stateful Haskell

correct_value_types_value_names_g = ( \vts vns -> 
  zipWith value_map_insert vns vts==>sequence_ >>
  paren_comma_sep_g show vns==>return
  ) :: [ ValueType ] -> [ ValueName ] -> Stateful Haskell
-- TupleMatching end

abstraction_g = ( \bt -> \case
  ValueNameAb vn -> value_map_insert vn vt >> show vn ==> return where
    vt = ( bt ==> \case
      ParenType(ParenVT vt) -> vt
      _ -> AbsTypesAndResType [] bt
      ) :: ValueType 
  TupleMatching tm -> tuple_matching_g bt tm
  ) :: BaseType -> Abstraction -> Stateful Haskell

-- Abstractions: abstractions_g, correct_abstractions_g
abstractions_g = ( \bts (As as) -> case length bts == length as of
  False -> error abstractions_types_lengths_dont_match_err
  True -> correct_abstractions_g bts as
  ) :: [ BaseType ] -> Abstractions -> Stateful Haskell

correct_abstractions_g = ( \bts -> \case
  [] -> return ""
  as ->
    zipWith bt_abstraction_g bts as==>sequence==>fmap concat >>= \as_g ->
    return $ "\\" ++ as_g ++ "-> "
    where
    bt_abstraction_g = ( \bt a -> abstraction_g bt a >>= (++ " ") .> return )
      :: BaseType -> Abstraction -> Stateful Haskell
  ) :: [ BaseType ] -> [ Abstraction ] -> Stateful Haskell
