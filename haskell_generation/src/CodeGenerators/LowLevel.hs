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
  ( Stateful, value_map_get, value_map_insert, type_map_get )

import CodeGenerators.ErrorMessages
  ( literal_not_int_err, type_check_err, tuple_matching_err
  , tuple_function_type_err, tuple_less_than_2_err
  , tuple_values_types_lengths_dont_match_err
  , abstractions_types_lengths_dont_match_err )

-- All:
-- Literal, LiteralOrValueName, TupleMatching, Abstraction, Abstractions

-- Literal: literal_g, literal_type_inference_g
literal_g = ( \vt l -> 
  (vt == NamedType (TN "Int")) ==> \case
    True -> return $ show l
    False -> undefined
  ) :: ValType -> Literal -> Stateful Haskell

literal_type_inference_g = ( \l -> return (NamedType $ TN "Int", show l) )
  :: Literal -> Stateful ( ValType, Haskell )

-- LiteralOrValueName:
-- literal_or_value_name_g, type_check_value_name_g, ts_are_equivalent,
-- tns_are_equivalent, tn_t_are_equivalent, tn_to_t
-- literal_or_value_name_type_inference_g
literal_or_value_name_g = ( \vt -> \case
  Literal l -> literal_g vt l
  ValueName vn ->
    value_map_get vn >>= \lookup_vt -> type_check_value_name_g vt lookup_vt vn
  ) :: ValType -> LiteralOrValueName -> Stateful Haskell

type_check_value_name_g = ( \vt lookup_vt vn ->
  ts_are_equivalent vt lookup_vt >>= \case
    False -> undefined
    True -> case vn of
      VN "true" -> return "True"
      VN "false" -> return "False"
      _ -> return $ show vn
  ) :: ValType -> ValType -> ValueName -> Stateful Haskell

ts_are_equivalent = ( \vt1 vt2 -> case (vt1, vt2) of

  (FunctionType in_vt1 out_vt1, FunctionType in_vt2 out_vt2) ->
    ts_are_equivalent in_vt1 in_vt2 >>= \in_equiv ->
    ts_are_equivalent out_vt1 out_vt2 >>= \out_equiv -> 
    return $ in_equiv && out_equiv
  (NamedType tn1, NamedType tn2) -> tns_are_equivalent tn1 tn2
  (TupleValType vt1_1 vt1_2 vts1, TupleValType vt2_1 vt2_2 vts2) ->
    zipWith ts_are_equivalent (vt1_1 : vt1_2 : vts1) (vt2_1 : vt2_2 : vts2)
      ==> sequence ==> fmap and

  (FunctionType _ _ ,NamedType _) -> return False
  (NamedType _, FunctionType _ _) -> return False

  (FunctionType _ _ ,TupleValType _ _ _) -> return False
  (TupleValType _ _ _, FunctionType _ _) -> return False

  (NamedType tn, TupleValType _ _ _) -> tn_t_are_equivalent tn vt2
  (TupleValType _ _ _, NamedType tn) -> tn_t_are_equivalent tn vt1

  ) :: ValType -> ValType -> Stateful Bool

tns_are_equivalent = ( \tn1 tn2 -> case tn1 == tn2 of
  True -> return True
  False -> tn_to_t tn1 >>= tn_t_are_equivalent tn2
  ) :: TypeName -> TypeName -> Stateful Bool

tn_t_are_equivalent = ( \tn vt -> tn_to_t tn >>= ts_are_equivalent vt
  ) :: TypeName -> ValType -> Stateful Bool

tn_to_t = ( type_map_get >=> \case
  FieldAndValTypeList favtl -> case favtl of
    [] -> undefined
    [ favt ] -> return $ get_f_valtype favt
    favt1 : favt2 : rest -> return $ TupleValType
      (get_f_valtype favt1) (get_f_valtype favt2) (map get_f_valtype rest)
  _ -> undefined
  ) :: TypeName -> Stateful ValType

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
  ) :: LiteralOrValueName -> Stateful ( ValType, Haskell )
-- TupleMatching:
-- tuple_matching_g
tuple_matching_g = ( \case
  -- possibly later with symbol table ?
  NamedType tn -> undefined
  FunctionType _ _ -> undefined
  TupleValType vt1 vt2 vts -> \(TM vn1 vn2 vns) -> case length vts == length vns of
    False -> undefined
    True -> 
      zipWith value_map_insert (vn1 : vn2 : vns) (vt1 : vt2 : vts)==>sequence_ >>
      return ("(" ++ map show (vn1 : vn2 : vns)==>intercalate ", " ++ ")")
  ) :: ValType -> TupleMatching -> Stateful Haskell

-- Abstraction: abstraction_g, use_fields_g
abstraction_g = ( \vt -> \case
  UseFields -> use_fields_g vt
  ValueNameAb vn -> value_map_insert vn vt >> show vn ==> return
  TupleMatching tm -> tuple_matching_g vt tm
  ) :: ValType -> Abstraction -> Stateful Haskell

use_fields_g = ( \vt -> case vt of
  NamedType tn -> type_map_get tn >>= \case
    FieldAndValTypeList fatl -> 
      value_map_insert (VN "value") vt >>
      mapM ( \(FVT vn vt) -> value_map_insert vn vt >> return vn ) fatl >>= \vns ->
      return $
        "value@(" ++ show tn ++ "C" ++ concatMap (show .> (" " ++)) vns ++ ")"
    _ -> undefined
  _ -> undefined 
  ) :: ValType -> Stateful Haskell

-- Abstractions: abstractions_g, correct_abstractions_g
abstractions_g = ( \vts as -> case length vts == length as of
  False -> error abstractions_types_lengths_dont_match_err
  True -> correct_abstractions_g vts as
  ) :: [ ValType ] -> [ Abstraction ] -> Stateful Haskell

correct_abstractions_g = ( \vts -> \case
  [] -> return ""
  as -> as_g >>= \as_h -> return $ "\\" ++ as_h ++ "-> " where
    as_g =
      zipWith abstraction_g vts as==>sequence==>fmap (concatMap (++ " "))
      :: Stateful Haskell
  ) :: [ ValType ] -> [ Abstraction ] -> Stateful Haskell
