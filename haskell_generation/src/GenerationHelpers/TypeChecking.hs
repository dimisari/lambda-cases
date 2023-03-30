module GenerationHelpers.TypeChecking where

import Control.Monad ((>=>))
import Control.Monad.Trans.Except (throwE)

import Helpers (Haskell, (==>))

import ParsingTypes.LowLevelValues (ValueName(..))
import ParsingTypes.LowLevelTypes (TypeName(..))

import AfterParsing.Types
import AfterParsing.Conversions 

import GenerationState.TypesAndOperations (Stateful, type_map_get)

import GenerationHelpers.ErrorMessages (type_check_err)

-- All:
-- types_are_equivalent, type_names_are_equivalent,
-- func_types_are_equivalent, prod_types_are_equivalent,
-- named_type_and_val_type_are_equivalent, type_name_to_val_type

types_are_equivalent = ( \val_type1 val_type2 -> case (val_type1, val_type2) of
  ( TypeApplication' (ConstructorAndInputs' type_name1 _) 
    , TypeApplication' (ConstructorAndInputs' type_name2 _)) ->
    type_names_are_equivalent type_name1 type_name2
  (ProductType' types1, ProductType' types2) ->
    prod_types_are_equivalent types1 types2
  (FunctionType' func_type1, FunctionType' func_type2) ->
    func_types_are_equivalent func_type1 func_type2
  _ -> return False
  -- (TypeName' type_name, ProductType' _) -> return False
  --   named_type_and_val_type_are_equivalent type_name val_type2
  -- (ProductType' _, TypeName' type_name) -> return False
  --   named_type_and_val_type_are_equivalent type_name val_type1
  ) :: ValueType' -> ValueType' -> Stateful Bool

type_names_are_equivalent = ( \type_name1 type_name2 ->
  case type_name1 == type_name2 of
    True -> return True
    False ->
      type_name_to_val_type type_name1 >>=
      named_type_and_val_type_are_equivalent type_name2
  ) :: TypeName -> TypeName -> Stateful Bool

func_types_are_equivalent = (
  \(InputAndOutputType' in_t1 out_t1) (InputAndOutputType' in_t2 out_t2) ->
  types_are_equivalent in_t1 in_t2 >>= \inputs_equivalent ->
  types_are_equivalent out_t1 out_t2 >>= \outputs_equivalent -> 
  return $ inputs_equivalent && outputs_equivalent
  ) :: FunctionType' -> FunctionType' -> Stateful Bool

prod_types_are_equivalent = ( \val_types1 val_types2 ->
  case length val_types1 == length val_types2 of
    True ->
      zipWith types_are_equivalent val_types1 val_types2==>sequence==>fmap and
    False ->
      return False
  ) :: [ ValueType' ] -> [ ValueType' ] -> Stateful Bool

named_type_and_val_type_are_equivalent = ( \type_name val_type ->
  type_name_to_val_type type_name >>= types_are_equivalent val_type
  ) :: TypeName -> ValueType' -> Stateful Bool

type_name_to_val_type = ( \type_name -> 
  type_map_get type_name >>= \case
    FieldList fields -> case fields of
      [] -> undefined
      [ favt ] -> return $ get_type favt
      favt1 : favt2 : rest -> return $ ProductType' $
        get_type favt1 : get_type favt2 : map get_type rest
    _ -> undefined
  ) :: TypeName -> Stateful ValueType'
