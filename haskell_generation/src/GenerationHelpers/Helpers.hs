module GenerationHelpers.Helpers where

import Data.List (intercalate)
import Control.Monad ((>=>), zipWithM)
import Text.Parsec (SourcePos)
import Control.Monad.Trans.Except (throwE, catchE)

import Helpers (Haskell, Pos(..))

import ParsingTypes.LowLevel (ValueName(..))
import ParsingTypes.Types (TypeName)

import IntermediateTypes.Types 
import IntermediateTypes.TypeDefinitions (TypeInfo(..), TTField(..))

import GenerationHelpers.TypeChecking
import GenerationHelpers.ErrorMessages

import GenerationState.TypesAndOperations


class GenerateInserted a where
  gen_inserted :: a -> Stateful (Haskell, [ ValueName ])

data ValNameType =
  ValNameType ValueName ValType

instance GenerateInserted ValNameType where
  gen_inserted = \(ValNameType val_name val_type) ->
    value_map_insert val_name val_type >> return (show val_name, [ val_name ])

data TTNameAndFields = 
  TTNameAndFields TypeName [ TTField ]

instance GenerateInserted TTNameAndFields where
  gen_inserted = \(TTNameAndFields type_name fields) ->
    unzip <$> mapM field_ins_and_ret_hs fields >>= \(fields_hs, inserted) ->
    return
      ( "@(C" ++ show type_name ++ concatMap (" " ++) fields_hs ++ ")"
      , inserted 
      )

-- matching

tuple_type_matching_g = ( \type_name fields ->
  unzip <$> mapM field_ins_and_ret_hs fields >>= \(fields_hs, inserted) ->
  return
    ( "@(C" ++ show type_name ++ concatMap (" " ++) fields_hs ++ ")"
    , inserted 
    )
  ) :: TypeName -> [ TTField ] -> Stateful (Haskell, [ ValueName ])

prod_type_matching_g = ( \types ->
  unzip <$> zipWithM map_ins_generate prod_t_field_ns types >>=
    \(fields_hs, inserted) ->
  return ("@(" ++ intercalate ", " fields_hs ++ ")", inserted)
  ) :: [ ValType ] -> Stateful (Haskell, [ ValueName ])

-- insert return

map_ins_generate = ( \val_name val_type ->
  value_map_insert val_name val_type >> return (show val_name, val_name)
  ) :: ValueName -> ValType -> Stateful (Haskell, ValueName)

field_ins_and_ret_hs = ( \(FNameAndType field_name field_type) ->
  map_ins_generate field_name field_type
  ) :: TTField -> Stateful (Haskell, ValueName)

-- 

prod_t_field_ns = map VN [ "first", "second", "third", "fourth", "fifth" ]
  :: [ ValueName ]

--  value in cases

maybe_value_g = ( \val_name val_type -> 
  value_map_get val_name >>= \val_name_t ->
  equiv_types val_name_t val_type >>= \case
    True -> return ("", [])
    False -> has_value_g val_name_t
  ) :: ValueName -> ValType -> Stateful (Haskell, [ ValueName ])

has_value_g = ( \case 
  FuncType (InAndOutTs in_t _) ->
    value_map_insert (VN "value") in_t >>
    value_matching_in_t_g in_t >>= \(value_matching_hs, inserted) ->
    return $ (" value" ++ value_matching_hs, VN "value" : inserted)
  _ -> error
    "case_with_value_vn_g: val_name_t not a FuncType, should be impossible"
  ) :: ValType -> Stateful (Haskell, [ ValueName ])

value_matching_in_t_g = ( \case
  TypeApp (ConsAndTIns type_name []) -> value_matching_type_name_g type_name
  ProdType types -> prod_type_matching_g types
  _ -> return ("", [])
  ) :: ValType -> Stateful (Haskell, [ ValueName ])

value_matching_type_name_g = ( \type_name ->
  type_map_get type_name >>= \case
    TupleType _ fields -> tuple_type_matching_g type_name fields
    _ -> return ("", [])
  ) :: TypeName -> Stateful (Haskell, [ ValueName ])

-- helpers

add_pos_to_err = ( \pos err -> case err of
  (False, err_t, err_msg) ->
    throwE $ (True, err_t, "\n" ++ show pos ++ "\n\n" ++ err_msg)
  _ -> throwE $ err
  ) :: SourcePos -> Error -> Stateful a

catchEaddPos = ( \g pos -> catchE g (add_pos_to_err pos) )
  :: Stateful a -> SourcePos -> Stateful a

