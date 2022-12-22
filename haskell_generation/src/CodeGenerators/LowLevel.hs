{-# language LambdaCase #-}

module CodeGenerators.LowLevel where

import qualified Data.Map as M
  ( lookup )

import Helpers
  ( Haskell, (==>), (.>), paren_comma_sep_g )

import HaskellTypes.LowLevel
  ( Literal(..), ValueName(..), LiteralOrValueName(..), TupleMatching(..)
  , Abstraction(..) )
import HaskellTypes.Types
  ( ParenType(..), BaseType(..), ValueType(..), TypeName(..) )
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
literal_g = ( \vt l -> case (vt == AbsTypesAndResType [] (TypeName (TN "Int"))) of
  True -> show l
  False -> error $ literal_not_int_err vt
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

vts_are_equivalent = (
  \vt1@(AbsTypesAndResType abs_ts1 bt1) vt2@(AbsTypesAndResType abs_ts2 bt2) ->
  case ( abs_ts1, bt1 ) of
    ( [], ParenType (ParenVT vt1_) ) -> vts_are_equivalent vt1_ vt2
    _ -> case ( abs_ts2, bt2 ) of
      ( [], ParenType (ParenVT vt2_) ) -> vts_are_equivalent vt1 vt2_
      _ -> case ( abs_ts1, abs_ts2 ) of
        ([], []) -> bts_are_equivalent bt1 bt2 
        (_, []) -> return False
        ([], _) -> return False
        (abs_t1 : rest1, abs_t2 : rest2) ->
          bts_are_equivalent abs_t1 abs_t2 >>= \bts_equiv ->
          vts_are_equivalent
            (AbsTypesAndResType rest1 bt1) (AbsTypesAndResType rest2 bt2)
            >>= \vts_equiv ->
          return $ bts_equiv && vts_equiv
  ) :: ValueType -> ValueType -> Stateful Bool

bts_are_equivalent = ( \bt1 bt2 -> case ( bt1, bt2 ) of
  ( ParenType (ParenVT (AbsTypesAndResType [] bt1_)), _ ) ->
    bts_are_equivalent bt1_ bt2
  ( _, ParenType (ParenVT (AbsTypesAndResType [] bt2_)) ) ->
    bts_are_equivalent bt1 bt2_
  ( TypeName tn1, TypeName tn2 ) -> tns_are_equivalent tn1 tn2
  ( TypeName tn, ParenType pt ) -> tn_pt_are_equivalent tn pt
  ( ParenType pt, TypeName tn ) -> tn_pt_are_equivalent tn pt
  ( ParenType pt1, ParenType pt2 ) -> pts_are_equivalent pt1 pt2
  ) :: BaseType -> BaseType -> Stateful Bool

tns_are_equivalent = ( \tn1 tn2 -> case tn1 == tn2 of 
  True -> return True
  False -> tn_to_pt tn1 >>= tn_pt_are_equivalent tn2
  ) :: TypeName -> TypeName -> Stateful Bool

tn_pt_are_equivalent = ( \tn pt -> tn_to_pt tn >>= pts_are_equivalent pt
  ) :: TypeName -> ParenType -> Stateful Bool

pts_are_equivalent = ( \pt1 pt2 -> case pt1 == pt2 of 
  True -> return True
  False -> undefined
  ) :: ParenType -> ParenType -> Stateful Bool

tn_to_pt = undefined
  :: TypeName -> Stateful ParenType

literal_or_value_name_type_inference_g = ( \case
  Literal l -> return $ literal_type_inference_g l
  ValueName vn -> value_map_get vn >>= \vt -> return ( vt, show vn )
  ) :: LiteralOrValueName -> Stateful ( ValueType, Haskell )

-- TupleMatching:
-- tuple_matching_g, value_type_tuple_matching_g, value_types_tuple_matching_g,
-- correct_value_types_value_names_g
tuple_matching_g = ( \case
  ParenType pt -> paren_tuple_matching_g pt
  -- possibly later with symbol table ?
  TypeName tn -> undefined
  ) :: BaseType -> TupleMatching -> Stateful Haskell

paren_tuple_matching_g = ( \case
  ParenVT vt -> value_type_tuple_matching_g vt
  TupleType vt1 vt2 vts -> value_types_tuple_matching_g $ vt1 : vt2 : vts 
  ) :: ParenType -> TupleMatching -> Stateful Haskell

value_type_tuple_matching_g = ( \case
  vt@(AbsTypesAndResType (_:_) _) -> error $ tuple_function_type_err vt
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
abstractions_g = ( \bts as -> case length bts == length as of
  False -> error abstractions_types_lengths_dont_match_err
  True -> correct_abstractions_g bts as
  ) :: [ BaseType ] -> [ Abstraction ] -> Stateful Haskell

correct_abstractions_g = ( \bts -> \case
  [] -> return ""
  as ->
    zipWith bt_abstraction_g bts as==>sequence==>fmap concat >>= \as_g ->
    return $ "\\" ++ as_g ++ "-> "
    where
    bt_abstraction_g = ( \bt a -> abstraction_g bt a >>= (++ " ") .> return )
      :: BaseType -> Abstraction -> Stateful Haskell
  ) :: [ BaseType ] -> [ Abstraction ] -> Stateful Haskell
