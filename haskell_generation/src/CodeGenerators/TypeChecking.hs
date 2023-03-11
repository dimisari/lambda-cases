{-# language LambdaCase #-}

module CodeGenerators.TypeChecking where

import Control.Monad 
  ( (>=>) )
import Helpers
  ( Haskell, (==>) )

import HaskellTypes.LowLevel
  ( ValueName(..) )
import HaskellTypes.Types
  ( TypeName(..) )
import HaskellTypes.AfterParsing 
  ( ValType(..), FieldAndValType(..), ValFieldsOrCases(..) )
import HaskellTypes.Generation
  ( Stateful, type_map_get )

-- All:
-- Literal, LiteralOrValueName, ManyAbstractions, Abstraction, Abstractions

type_check_value_name_g = ( \vt lookup_vt vn ->
  ts_are_equivalent vt lookup_vt >>= \case
    False -> undefined
    True -> case vn of
      VN "true" -> return "True"
      VN "false" -> return "False"
      _ -> return $ show vn
  ) :: ValType -> ValType -> ValueName -> Stateful Haskell

ts_are_equivalent = ( \vt1 vt2 -> case (vt1, vt2) of

  (FuncType in_vt1 out_vt1, FuncType in_vt2 out_vt2) ->
    ts_are_equivalent in_vt1 in_vt2 >>= \in_equiv ->
    ts_are_equivalent out_vt1 out_vt2 >>= \out_equiv -> 
    return $ in_equiv && out_equiv
  (NamedType tn1, NamedType tn2) -> tns_are_equivalent tn1 tn2
  (TupleValType vt1_1 vt1_2 vts1, TupleValType vt2_1 vt2_2 vts2) ->
    zipWith ts_are_equivalent (vt1_1 : vt1_2 : vts1) (vt2_1 : vt2_2 : vts2)
      ==> sequence ==> fmap and

  (FuncType _ _ ,NamedType _) -> return False
  (NamedType _, FuncType _ _) -> return False

  (FuncType _ _ ,TupleValType _ _ _) -> return False
  (TupleValType _ _ _, FuncType _ _) -> return False

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
