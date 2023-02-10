{-# language LambdaCase #-}

module CodeGenerators.LowLevel where

import Data.List
  ( intercalate )
import qualified Data.Map as M
  ( lookup )
import Control.Monad
  ( (>=>) )

import Helpers
  ( Haskell, (==>), (.>) )

import HaskellTypes.LowLevel
  ( Literal(..), ValueName(..), LiteralOrValueName(..), TupleMatching(..)
  , Abstraction(..) )
import HaskellTypes.Types
  ( BaseType(..), ValueType(..), TypeName(..), FieldsOrCases(..)
  , FieldAndType(..) )
import HaskellTypes.AfterParsing
  ( ValType(..), ValFieldsOrCases(..), FieldAndValType(..) )
import HaskellTypes.Generation
  ( Stateful, val_map_get, val_map_insert, val_type_map_get, value_map_get
  , value_map_insert, type_map_get )

import CodeGenerators.ErrorMessages
  ( literal_not_int_err, type_check_err, tuple_matching_err
  , tuple_function_type_err, tuple_less_than_2_err
  , tuple_values_types_lengths_dont_match_err
  , abstractions_types_lengths_dont_match_err )

-- All:
-- Literal, LiteralOrValueName, TupleMatching, Abstraction, Abstractions

-- Literal: literal_g, literal_type_inference_g
literal_g = ( \vt l -> 
  vts_are_equivalent vt (AbsTypesAndResType [] (TypeName (TN "Int"))) >>= \case
    True -> return $ show l
    False -> error $ literal_not_int_err vt
  ) :: ValueType -> Literal -> Stateful Haskell

literal_type_inference_g = ( \l -> 
  return (AbsTypesAndResType [] (TypeName (TN "Int")), show l)
  ) :: Literal -> Stateful ( ValueType, Haskell )

-- (ValType) Literal: val_literal_g, val_literal_type_inference_g
val_literal_g = ( \vt l -> 
  (vt == NamedType (TN "Int")) ==> \case
    True -> return $ show l
    False -> undefined
  ) :: ValType -> Literal -> Stateful Haskell

val_literal_type_inference_g = ( \l -> return (NamedType $ TN "Int", show l) )
  :: Literal -> Stateful ( ValType, Haskell )

-- LiteralOrValueName:
-- literal_or_value_name_g, type_check_value_name_g, vts_are_equivalent, 
-- bts_are_equivalent, tns_are_equivalent, tn_bt_are_equivalent, tn_to_bt
-- literal_or_value_name_type_inference_g
literal_or_value_name_g = ( \vt -> \case
  Literal l -> literal_g vt l
  ValueName vn ->
    value_map_get vn >>= \lookup_vt -> type_check_value_name_g vt lookup_vt vn
  ) :: ValueType -> LiteralOrValueName -> Stateful Haskell

type_check_value_name_g = ( \vt lookup_vt vn ->
  vts_are_equivalent vt lookup_vt >>= \case
    False -> error $ type_check_err vn lookup_vt vt 
    True -> case vn of
      VN "true" -> return "True"
      VN "false" -> return "False"
      _ -> return $ show vn
  ) :: ValueType -> ValueType -> ValueName -> Stateful Haskell

vts_are_equivalent = (
  \vt1@(AbsTypesAndResType abs_ts1 bt1) vt2@(AbsTypesAndResType abs_ts2 bt2) ->
  case (abs_ts1, bt1, abs_ts2, bt2) of
    ([], ParenType vt1_, _, _) -> vts_are_equivalent vt1_ vt2
    (_, _, [], ParenType vt2_) -> vts_are_equivalent vt1 vt2_
    _ -> case ( abs_ts1, abs_ts2 ) of
      ([], []) -> bts_are_equivalent bt1 bt2 
      (_, []) -> return False
      ([], _) -> return False
      (abs_t1 : rest1, abs_t2 : rest2) ->
        bts_are_equivalent abs_t1 abs_t2
          >>= \bts_equiv ->
        vts_are_equivalent
          (AbsTypesAndResType rest1 bt1) (AbsTypesAndResType rest2 bt2)
          >>= \vts_equiv ->
        return $ bts_equiv && vts_equiv
  ) :: ValueType -> ValueType -> Stateful Bool

bts_are_equivalent = ( \bt1 bt2 -> case ( bt1, bt2 ) of
  (ParenType (AbsTypesAndResType [] bt1_), _) -> bts_are_equivalent bt1_ bt2
  (_, ParenType (AbsTypesAndResType [] bt2_)) -> bts_are_equivalent bt1 bt2_

  (TypeName tn1, TypeName tn2) -> tns_are_equivalent tn1 tn2
  (ParenType vt1, ParenType vt2) -> vts_are_equivalent vt1 vt2
  (TupleType vt1 vt2 rest, TupleType vt1_ vt2_ rest_) ->
   zipWith vts_are_equivalent (vt1:vt2:rest) (vt1_:vt2_:rest_)
     ==> sequence ==> fmap and

  (TypeName _, ParenType _) -> return False
  (ParenType _, TypeName _) -> return False

  (TypeName tn, TupleType _ _ _) -> tn_bt_are_equivalent tn bt2
  (TupleType _ _ _, TypeName tn) -> tn_bt_are_equivalent tn bt1

  (TupleType _ _ _, ParenType _) -> return False
  (ParenType _, TupleType _ _ _) -> return False

  ) :: BaseType -> BaseType -> Stateful Bool

tns_are_equivalent = ( \tn1 tn2 -> case tn1 == tn2 of 
  True -> return True
  False -> tn_to_bt tn1 >>= tn_bt_are_equivalent tn2
  ) :: TypeName -> TypeName -> Stateful Bool

tn_bt_are_equivalent = ( \tn bt -> tn_to_bt tn >>= bts_are_equivalent bt
  ) :: TypeName -> BaseType -> Stateful Bool

tn_to_bt = ( type_map_get >=> \case
  FieldAndTypeList fatl -> case fatl of
    [] -> undefined
    [ fat ] -> return $ ParenType $ get_field_type fat
    fat1 : fat2 : rest ->
      return $ TupleType
        (get_field_type fat1) (get_field_type fat2) (map get_field_type rest)
  CaseAndMaybeTypeList _ -> undefined
  ) :: TypeName -> Stateful BaseType

literal_or_value_name_type_inference_g = ( \case
  Literal l -> literal_type_inference_g l
  ValueName vn -> value_map_get vn >>= \vt ->
    let
    hs = case vn of
      VN "true" -> "True"
      VN "false" -> "False"
      _ -> show vn
    in
    return ( vt, hs )
  ) :: LiteralOrValueName -> Stateful ( ValueType, Haskell )

-- (ValType) LiteralOrValueName:
-- val_literal_or_value_name_g, val_type_check_value_name_g, val_ts_are_equivalent,
-- val_tns_are_equivalent, tn_val_t_are_equivalent, tn_to_val_t
-- val_literal_or_value_name_type_inference_g
val_literal_or_value_name_g = ( \vt -> \case
  Literal l -> val_literal_g vt l
  ValueName vn ->
    val_map_get vn >>= \lookup_vt -> val_type_check_value_name_g vt lookup_vt vn
  ) :: ValType -> LiteralOrValueName -> Stateful Haskell

val_type_check_value_name_g = ( \vt lookup_vt vn ->
  val_ts_are_equivalent vt lookup_vt >>= \case
    False -> undefined
    True -> case vn of
      VN "true" -> return "True"
      VN "false" -> return "False"
      _ -> return $ show vn
  ) :: ValType -> ValType -> ValueName -> Stateful Haskell

val_ts_are_equivalent = ( \vt1 vt2 -> case (vt1, vt2) of

  (FunctionType in_vt1 out_vt1, FunctionType in_vt2 out_vt2) ->
    val_ts_are_equivalent in_vt1 in_vt2 >>= \in_equiv ->
    val_ts_are_equivalent out_vt1 out_vt2 >>= \out_equiv -> 
    return $ in_equiv && out_equiv
  (NamedType tn1, NamedType tn2) -> val_tns_are_equivalent tn1 tn2
  (TupleValType vt1_1 vt1_2 vts1, TupleValType vt2_1 vt2_2 vts2) ->
    zipWith val_ts_are_equivalent (vt1_1 : vt1_2 : vts1) (vt2_1 : vt2_2 : vts2)
      ==> sequence ==> fmap and

  (FunctionType _ _ ,NamedType _) -> return False
  (NamedType _, FunctionType _ _) -> return False

  (FunctionType _ _ ,TupleValType _ _ _) -> return False
  (TupleValType _ _ _, FunctionType _ _) -> return False

  (NamedType tn, TupleValType _ _ _) -> tn_val_t_are_equivalent tn vt2
  (TupleValType _ _ _, NamedType tn) -> tn_val_t_are_equivalent tn vt1

  ) :: ValType -> ValType -> Stateful Bool

val_tns_are_equivalent = ( \tn1 tn2 -> case tn1 == tn2 of
  True -> return True
  False -> tn_to_val_t tn1 >>= tn_val_t_are_equivalent tn2
  ) :: TypeName -> TypeName -> Stateful Bool

tn_val_t_are_equivalent = ( \tn vt -> tn_to_val_t tn >>= val_ts_are_equivalent vt
  ) :: TypeName -> ValType -> Stateful Bool

tn_to_val_t = ( val_type_map_get >=> \case
  FieldAndValTypeList favtl -> case favtl of
    [] -> undefined
    [ favt ] -> return $ get_f_valtype favt
    favt1 : favt2 : rest -> return $ TupleValType
      (get_f_valtype favt1) (get_f_valtype favt2) (map get_f_valtype rest)
  _ -> undefined
  ) :: TypeName -> Stateful ValType

val_literal_or_value_name_type_inference_g = ( \case
  Literal l -> val_literal_type_inference_g l
  ValueName vn -> val_map_get vn >>= \vt ->
    let
    hs = case vn of
      VN "true" -> "True"
      VN "false" -> "False"
      _ -> show vn
    in
    return ( vt, hs )
  ) :: LiteralOrValueName -> Stateful ( ValType, Haskell )

-- TupleMatching:
-- tuple_matching_g, value_type_tuple_matching_g, value_types_tuple_matching_g,
-- correct_value_types_value_names_g
tuple_matching_g = ( \case
  -- possibly later with symbol table ?
  TypeName tn -> undefined
  ParenType vt -> value_type_tuple_matching_g vt
  TupleType vt1 vt2 vts -> value_types_tuple_matching_g $ vt1 : vt2 : vts 
  ) :: BaseType -> TupleMatching -> Stateful Haskell

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
  return ("(" ++ intercalate ", " (map show vns) ++ ")")
  ) :: [ ValueType ] -> [ ValueName ] -> Stateful Haskell

-- (ValType) TupleMatching:
-- val_tuple_matching_g
val_tuple_matching_g = ( \case
  -- possibly later with symbol table ?
  NamedType tn -> undefined
  FunctionType _ _ -> undefined
  TupleValType vt1 vt2 vts -> \(TM vn1 vn2 vns) -> case length vts == length vns of
    False -> undefined
    True -> 
      zipWith val_map_insert (vn1 : vn2 : vns) (vt1 : vt2 : vts)==>sequence_ >>
      return ("(" ++ map show (vn1 : vn2 : vns)==>intercalate ", " ++ ")")
  ) :: ValType -> TupleMatching -> Stateful Haskell

-- Abstraction: abstraction_g
abstraction_g = ( \bt -> \case
  ValueNameAb vn -> value_map_insert vn vt >> show vn ==> return where
    vt = ( bt ==> \case
      ParenType vt -> vt
      _ -> AbsTypesAndResType [] bt
      ) :: ValueType 
  TupleMatching tm -> tuple_matching_g bt tm
  ) :: BaseType -> Abstraction -> Stateful Haskell

-- (ValType) Abstraction: val_abstraction_g
val_abstraction_g = ( \vt -> \case
  ValueNameAb vn -> val_map_insert vn vt >> show vn ==> return where
  TupleMatching tm -> val_tuple_matching_g vt tm
  ) :: ValType -> Abstraction -> Stateful Haskell

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

-- (ValType) Abstractions: val_abstractions_g, val_correct_abstractions_g
val_abstractions_g = ( \vts as -> case length vts == length as of
  False -> error abstractions_types_lengths_dont_match_err
  True -> val_correct_abstractions_g vts as
  ) :: [ ValType ] -> [ Abstraction ] -> Stateful Haskell

val_correct_abstractions_g = ( \vts -> \case
  [] -> return ""
  as -> as_g >>= \as_h -> return $ "\\" ++ as_h ++ "-> " where
    as_g =
      zipWith val_abstraction_g vts as==>sequence==>fmap (concatMap (++ " "))
      :: Stateful Haskell
  ) :: [ ValType ] -> [ Abstraction ] -> Stateful Haskell
