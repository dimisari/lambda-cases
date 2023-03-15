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

import CodeGenerators.ErrorMessages
  ( type_check_err )

-- All:
-- type_check_value_name_g, types_are_equivalent, type_names_are_equivalent,
-- func_types_are_equivalent, prod_types_are_equivalent,
-- named_type_and_val_type_are_equivalent, type_name_to_val_type

type_check_value_name_g = ( \value_name val_type map_val_type ->
  types_are_equivalent val_type map_val_type >>= \case
    False -> error $ type_check_err value_name val_type map_val_type
    True -> case value_name of
      VN "true" -> return "True"
      VN "false" -> return "False"
      _ -> return $ show value_name
  ) :: ValueName -> ValType -> ValType -> Stateful Haskell

types_are_equivalent = ( \val_type1 val_type2 -> case (val_type1, val_type2) of

  (NamedType type_name_1, NamedType type_name2) ->
    type_names_are_equivalent type_name_1 type_name2
  (ProdType t1_1 t1_2 ts1, ProdType t2_1 t2_2 ts2) ->
    prod_types_are_equivalent (t1_1 : t1_2 : ts1) (t2_1 : t2_2 : ts2)
  (FuncType input_t1 output_t1, FuncType input_t2 output_t2) ->
    func_types_are_equivalent (input_t1, output_t1) (input_t2, output_t2)

  (NamedType type_name, ProdType _ _ _) ->
    named_type_and_val_type_are_equivalent type_name val_type2
  (ProdType _ _ _, NamedType type_name) ->
    named_type_and_val_type_are_equivalent type_name val_type1

  (FuncType _ _ ,NamedType _) -> return False
  (NamedType _, FuncType _ _) -> return False

  (FuncType _ _ ,ProdType _ _ _) -> return False
  (ProdType _ _ _, FuncType _ _) -> return False

  ) :: ValType -> ValType -> Stateful Bool

type_names_are_equivalent = ( \type_name_1 type_name2 ->
  case type_name_1 == type_name2 of
    True -> return True
    False ->
      type_name_to_val_type type_name_1 >>=
      named_type_and_val_type_are_equivalent type_name2
  ) :: TypeName -> TypeName -> Stateful Bool

func_types_are_equivalent = (
  \(input_type1, output_type1) (input_type2, output_type2) ->
  types_are_equivalent input_type1 input_type2 >>= \inputs_equivalent ->
  types_are_equivalent output_type1 output_type2 >>= \outputs_equivalent -> 
  return $ inputs_equivalent && outputs_equivalent
  ) :: (ValType, ValType) -> (ValType, ValType) -> Stateful Bool

prod_types_are_equivalent = ( \val_types1 val_types2 ->
  case length val_types1 == length val_types2 of
    True ->
      zipWith types_are_equivalent val_types1 val_types2==>sequence==>fmap and
    False -> undefined
  ) :: [ ValType ] -> [ ValType ] -> Stateful Bool

named_type_and_val_type_are_equivalent = ( \type_name val_type ->
  type_name_to_val_type type_name >>= types_are_equivalent val_type
  ) :: TypeName -> ValType -> Stateful Bool

type_name_to_val_type = ( flip type_map_get "type_name_to_val_type" >=> \case
  FieldAndValTypeList favtl -> case favtl of
    [] -> undefined
    [ favt ] -> return $ get_f_valtype favt
    favt1 : favt2 : rest -> return $ ProdType
      (get_f_valtype favt1) (get_f_valtype favt2) (map get_f_valtype rest)
  _ -> undefined
  ) :: TypeName -> Stateful ValType
