module CodeGenerators.LowLevel where

import Data.List (intercalate)
import Control.Monad.Trans.Except (throwE)

import Helpers (Haskell, (==>))

import ParsingTypes.LowLevel
import ParsingTypes.Types (TypeName(..))

import IntermediateTypes.Types
import IntermediateTypes.TypeDefinitions (Field'(..), TypeInfo(..), int)

import GenerationState.TypesAndOperations

import GenerationHelpers.ErrorMessages
import GenerationHelpers.TypeChecking (types_are_equivalent)

-- All: Literal, ValueName, Abstraction, ManyAbstractions

-- Literal: literal_g, literal_type_inference_g

literal_g = ( \literal value_type -> 
  (value_type == int) ==> \case
    True -> return $ show literal
    False -> throwE $ literal_not_int_err value_type
  ) :: Literal -> ValType -> Stateful Haskell

literal_type_inference_g = ( \literal -> return (show literal, int) )
  :: Literal -> Stateful (Haskell, ValType)

-- ValueName: value_name_g, value_name_type_inference_g

value_name_g = ( \value_name value_type -> 
  value_map_get value_name >>= \map_val_type ->
  types_are_equivalent value_type map_val_type >>= \case
    False -> throwE $ type_check_err value_name value_type map_val_type
    True -> check_vn_in_or_t_cs_g value_name
  ) :: ValueName -> ValType -> Stateful Haskell

value_name_type_inference_g = ( \value_name -> 
  value_map_get value_name >>= \map_val_type ->
  check_vn_in_or_t_cs_g value_name >>= \value_name_hs ->
  return (value_name_hs, map_val_type)
  ) :: ValueName -> Stateful (Haskell, ValType)

check_vn_in_or_t_cs_g = ( \value_name -> in_or_t_cs value_name >>= \case
  True -> return $ "C" ++ show value_name
  _ -> return $ show value_name
  ) :: ValueName -> Stateful Haskell

-- Abstraction:
-- abstraction_g, val_name_ins_and_ret, use_fields_g, field_and_val_type_g

abstraction_g = ( \case
  AbstractionName value_name -> val_name_ins_and_ret value_name
  UseFields -> use_fields_g
  ) :: Abstraction -> ValType -> Stateful Haskell

val_name_ins_and_ret = ( \value_name value_type ->
  value_map_insert value_name value_type >> return (show value_name)
  ) :: ValueName -> ValType -> Stateful Haskell

use_fields_g = ( \value_type -> case value_type of
  TypeApp (TypeConsAndInputs' type_name _) ->
    use_fields_type_name_g type_name value_type
  ProdType types -> use_fields_prod_type_g types value_type
  _ -> undefined 
  ) :: ValType -> Stateful Haskell

use_fields_type_name_g = ( \type_name value_type ->
  type_map_get type_name >>= \case
    TupleType _ fields -> 
      value_map_insert (VN "tuple") value_type >>
      mapM field_and_val_type_g fields >>= \val_names ->
      return $
        "tuple@(C" ++ show type_name ++ concatMap (" " ++) val_names ++ ")"
    _ -> undefined
  ) :: TypeName -> ValType -> Stateful Haskell

use_fields_prod_type_g = ( \types value_type ->
  value_map_insert (VN "tuple") value_type >>
  zipWith val_name_ins_and_ret prod_type_fields types ==> sequence >>=
    \val_names ->
  return $ "(" ++ intercalate ", " val_names ++ ")"
  ) :: [ ValType ] -> ValType -> Stateful Haskell

prod_type_fields = map VN [ "first", "second", "third", "fourth", "fifth" ]
  :: [ ValueName ]

field_and_val_type_g = ( \(NameAndType' field_name field_type) ->
  val_name_ins_and_ret field_name field_type
  ) :: Field' -> Stateful Haskell

-- ManyAbstractions: many_abstractions_g

many_abstractions_g = ( \(Abstractions abstraction1 abstraction2 abstractions) ->
  abstractions_g (abstraction1 : abstraction2 : abstractions)
  ) :: ManyAbstractions -> ValType -> Stateful (ValType, Haskell)

-- Input: input_g, abstractions_g

input_g = ( \input value_type ->
  let 
  input_help_g = case input of
    OneAbstraction abstraction -> abstractions_g [ abstraction ] value_type 
    ManyAbstractions many_abs -> many_abstractions_g many_abs value_type
    :: Stateful (ValType, Haskell)
  in
  input_help_g >>= \(rest_t, help_hs) -> return (rest_t, "\\" ++ help_hs ++ "-> ")
  ) :: Input -> ValType -> Stateful (ValType, Haskell)

abstractions_g = ( \abstractions value_type -> case abstractions of
  [] -> return (value_type, "")
  abs1 : other_abs -> case value_type of
    FuncType (InAndOutTs input_type output_type) -> 
      abstraction_g abs1 input_type >>= \abs1_hs ->
      abstractions_g other_abs output_type >>= \(abs_type, other_abs_hs) ->
      return (abs_type, abs1_hs ++ " " ++ other_abs_hs)
    _ -> undefined
  ) :: [ Abstraction ] -> ValType -> Stateful (ValType, Haskell)
