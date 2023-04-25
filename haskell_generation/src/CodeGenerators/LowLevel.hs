module CodeGenerators.LowLevel where

import Data.List (intercalate)
import Control.Monad ((>=>))
import Control.Monad.Trans.Except (throwE)

import Helpers (Haskell, (==>), (.>))

import ParsingTypes.LowLevel
import ParsingTypes.Types (TypeName(..))

import IntermediateTypes.Types
import IntermediateTypes.TypeDefinitions (Field'(..), TypeInfo(..))

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
-- abstraction_g, val_n_ins_and_ret_hs, use_fields_g, field_ins_and_ret_hs

abstraction_g = ( \case
  AbstractionName value_name -> val_n_ins_and_ret_hs value_name
  UseFields -> use_fields_g
  ) :: Abstraction -> ValType -> Stateful Haskell

val_n_ins_and_ret_hs = ( \value_name value_type ->
  value_map_insert value_name value_type >> return (show value_name)
  ) :: ValueName -> ValType -> Stateful Haskell

use_fields_g = ( \value_type -> case value_type of
  TypeApp (ConsAndInTs type_name _) ->
    use_fields_type_name_g type_name value_type
  ProdType types -> use_fields_prod_type_g types value_type
  _ -> undefined 
  ) :: ValType -> Stateful Haskell

use_fields_type_name_g = ( \type_name value_type ->
  type_map_get type_name >>= \case
    TupleType _ fields -> 
      value_map_insert (VN "tuple") value_type >>
      mapM field_ins_and_ret_hs fields >>= \val_names_hs ->
      return $
        "tuple@(C" ++ show type_name ++ concatMap (" " ++) val_names_hs ++ ")"
    _ -> undefined
  ) :: TypeName -> ValType -> Stateful Haskell

use_fields_prod_type_g = ( \types value_type ->
  value_map_insert (VN "tuple") value_type >>
  zipWith val_n_ins_and_ret_hs prod_t_field_ns types ==> sequence >>=
    \val_names_hs ->
  return $ "tuple@(" ++ intercalate ", " val_names_hs ++ ")"
  ) :: [ ValType ] -> ValType -> Stateful Haskell

prod_t_field_ns = map VN [ "first", "second", "third", "fourth", "fifth" ]
  :: [ ValueName ]

field_ins_and_ret_hs = ( \(NameAndType' field_name field_type) ->
  val_n_ins_and_ret_hs field_name field_type
  ) :: Field' -> Stateful Haskell

abs_val_map_remove = ( \case
  AbstractionName val_name -> value_map_remove val_name
  UseFields -> use_fs_map_remove
  ) :: Abstraction -> Stateful ()

use_fs_map_remove = ( 
  value_map_get (VN "tuple") >>= \tuple_t -> 
  value_map_remove (VN "tuple") >> case tuple_t of
    TypeApp (ConsAndInTs type_name _) -> use_fs_tn_map_remove type_name 
    ProdType types -> mapM_ value_map_remove $ take (length types) prod_t_field_ns
    _ -> error "use_fs_map_remove: should be impossible"
  ) :: Stateful ()

use_fs_tn_map_remove = ( type_map_get >=> \case
  TupleType _ fields -> mapM_ (get_name .> value_map_remove) fields 
  _ -> undefined
  ) :: TypeName -> Stateful ()

-- ManyAbstractions: many_abstractions_g

many_abstractions_g = ( \(Abstractions abs1 abs2 abstractions) ->
  abstractions_g (abs1 : abs2 : abstractions)
  ) :: ManyAbstractions -> ValType -> Stateful (ValType, Haskell)

many_abs_val_map_remove = ( \(Abstractions abs1 abs2 abstractions) ->
  mapM_ abs_val_map_remove $ abs1 : abs2 : abstractions
  ) :: ManyAbstractions -> Stateful ()

-- Input: input_g, abstractions_g

input_g = ( \input value_type ->
  let 
  input_help_g = case input of
    OneAbstraction abstraction -> abstractions_g [ abstraction ] value_type 
    ManyAbstractions many_abs -> many_abstractions_g many_abs value_type
    :: Stateful (ValType, Haskell)
  in
  input_help_g >>= \(final_t, input_hs) ->
  return (final_t, "\\" ++ input_hs ++ "-> ")
  ) :: Input -> ValType -> Stateful (ValType, Haskell)

abstractions_g = ( \case
  [] -> \value_type -> return (value_type, "")
  abs1 : other_abs -> \case
    FuncType (InAndOutTs in_t out_t) -> 
      abstraction_g abs1 in_t >>= \abs1_hs ->
      abstractions_g other_abs out_t >>= \(final_t, other_abs_hs) ->
      return (final_t, abs1_hs ++ " " ++ other_abs_hs)
    _ -> undefined
  ) :: [ Abstraction ] -> ValType -> Stateful (ValType, Haskell)

input_val_map_remove = ( \case
  OneAbstraction abs -> abs_val_map_remove abs
  ManyAbstractions many_abs -> many_abs_val_map_remove many_abs
  ) :: Input -> Stateful ()
