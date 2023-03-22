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
  ( ValType(..), FuncType(..), FieldAndValType(..), ValFieldsOrCases(..) )
import HaskellTypes.Generation
  ( Stateful, type_map_get )

import CodeGenerators.ErrorMessages
  ( type_check_err )

-- All:
-- types_are_equivalent, type_names_are_equivalent,
-- func_types_are_equivalent, prod_types_are_equivalent,
-- named_type_and_val_type_are_equivalent, type_name_to_val_type

types_are_equivalent = ( \val_type1 val_type2 -> case (val_type1, val_type2) of

  (NamedType type_name1, NamedType type_name2) ->
    type_names_are_equivalent type_name1 type_name2
  (ProdType types1, ProdType types2) ->
    prod_types_are_equivalent types1 types2
  (FuncType func_type1, FuncType func_type2) ->
    func_types_are_equivalent func_type1 func_type2

  (NamedType type_name, ProdType _) ->
    named_type_and_val_type_are_equivalent type_name val_type2
  (ProdType _, NamedType type_name) ->
    named_type_and_val_type_are_equivalent type_name val_type1

  (FuncType _ ,NamedType _) -> return False
  (NamedType _, FuncType _) -> return False

  (FuncType _ ,ProdType _) -> return False
  (ProdType _, FuncType _) -> return False

  ) :: ValType -> ValType -> Stateful Bool

type_names_are_equivalent = ( \type_name1 type_name2 ->
  case type_name1 == type_name2 of
    True -> return True
    False ->
      type_name_to_val_type type_name1 >>=
      named_type_and_val_type_are_equivalent type_name2
  ) :: TypeName -> TypeName -> Stateful Bool

func_types_are_equivalent = (
  \(InAndOutType in_t1 out_t1) (InAndOutType in_t2 out_t2) ->
  types_are_equivalent in_t1 in_t2 >>= \inputs_equivalent ->
  types_are_equivalent out_t1 out_t2 >>= \outputs_equivalent -> 
  return $ inputs_equivalent && outputs_equivalent
  ) :: FuncType -> FuncType -> Stateful Bool

prod_types_are_equivalent = ( \val_types1 val_types2 ->
  case length val_types1 == length val_types2 of
    True ->
      zipWith types_are_equivalent val_types1 val_types2==>sequence==>fmap and
    False -> return False
  ) :: [ ValType ] -> [ ValType ] -> Stateful Bool

named_type_and_val_type_are_equivalent = ( \type_name val_type ->
  type_name_to_val_type type_name >>= types_are_equivalent val_type
  ) :: TypeName -> ValType -> Stateful Bool

type_name_to_val_type = ( flip type_map_get "type_name_to_val_type" >=> \case
  FieldAndValTypeList favtl -> case favtl of
    [] -> undefined
    [ favt ] -> return $ get_f_valtype favt
    favt1 : favt2 : rest -> return $ ProdType $
      get_f_valtype favt1 : get_f_valtype favt2 : map get_f_valtype rest
  _ -> undefined
  ) :: TypeName -> Stateful ValType
