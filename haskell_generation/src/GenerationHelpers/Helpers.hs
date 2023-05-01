module GenerationHelpers.Helpers where

import Data.List (intercalate)
import Control.Monad ((>=>), zipWithM)

import Helpers (Haskell)

import ParsingTypes.LowLevel (ValueName(..))
import ParsingTypes.Types (TypeName)

import IntermediateTypes.Types 
import IntermediateTypes.TypeDefinitions (TypeInfo(..), TTField(..))

import GenerationHelpers.TypeChecking

import GenerationState.TypesAndOperations


type_name_matching_g = ( \not_tuple_type_g type_name ->
  type_map_get type_name >>= \case
    TupleType _ fields -> tuple_type_matching_g type_name fields
    _ -> not_tuple_type_g
  ) :: Stateful Haskell -> TypeName -> Stateful Haskell

tuple_type_matching_g = ( \type_name fields ->
  mapM field_ins_and_ret_hs fields >>= \fields_hs ->
  return $ "@(C" ++ show type_name ++ concatMap (" " ++) fields_hs ++ ")"
  ) :: TypeName -> [ TTField ] -> Stateful Haskell

prod_type_matching_g = ( \types ->
  zipWithM val_n_ins_and_ret_hs prod_t_field_ns types >>= \fields_hs ->
  return $ "@(" ++ intercalate ", " fields_hs ++ ")"
  ) :: [ ValType ] -> Stateful Haskell

val_n_ins_and_ret_hs = ( \val_name val_type ->
  value_map_insert val_name val_type >> return (show val_name)
  ) :: ValueName -> ValType -> Stateful Haskell

field_ins_and_ret_hs = ( \(FNameAndType field_name field_type) ->
  val_n_ins_and_ret_hs field_name field_type
  ) :: TTField -> Stateful Haskell

prod_t_field_ns = map VN [ "first", "second", "third", "fourth", "fifth" ]
  :: [ ValueName ]

maybe_value_g = ( \val_name val_type -> 
  value_map_get val_name >>= \val_name_t ->
  equiv_types val_name_t val_type >>= \case
    True -> return ""
    False -> has_value_g val_name_t
  ) :: ValueName -> ValType -> Stateful Haskell

has_value_g = ( \case 
  FuncType (InAndOutTs in_t _) ->
    value_map_insert (VN "value") in_t >>
    value_matching_in_t_g in_t >>= \value_matching_hs ->
    return $ " value" ++ value_matching_hs
  _ -> error
    "case_with_value_vn_g: val_name_t not a FuncType, should be impossible"
  ) :: ValType -> Stateful Haskell

value_matching_in_t_g = ( \case
  TypeApp (ConsAndTIns type_name []) -> value_matching_type_name_g type_name
  ProdType types -> prod_type_matching_g types
  _ -> return ""
  ) :: ValType -> Stateful Haskell

value_matching_type_name_g = type_name_matching_g (return "")
  :: TypeName -> Stateful Haskell

