module GenerationHelpers.TypeChecking where

import Helpers (Haskell, (==>))

import ParsingTypes.LowLevel (ValueName(..))
import ParsingTypes.Types (TypeName(..))

import IntermediateTypes.Types
import IntermediateTypes.TypeDefinitions (TypeInfo(..), get_type)

import GenerationState.TypesAndOperations (Stateful, type_map_get)

-- All:
-- equiv_types, equiv_type_names,
-- equiv_func_types, equiv_prod_types,
-- equiv_t_name_val_t, t_name_to_val_t

equiv_types = ( \val_type1 val_type2 -> case (val_type1, val_type2) of
  (TypeApp (ConsAndInTs t_name1 _), TypeApp (ConsAndInTs t_name2 _)) ->
    equiv_type_names t_name1 t_name2
  (ProdType types1, ProdType types2) ->
    equiv_prod_types types1 types2
  (FuncType func_type1, FuncType func_type2) ->
    equiv_func_types func_type1 func_type2
  _ -> return False

  -- (TypeName' type_name, ProdType _) -> return False
  --   equiv_t_name_val_t type_name val_type2
  -- (ProdType _, TypeName' type_name) -> return False
  --   equiv_t_name_val_t type_name val_type1
  ) :: ValType -> ValType -> Stateful Bool

equiv_type_names = ( \type_name1 type_name2 ->
  case type_name1 == type_name2 of
    True -> return True
    False ->
      t_name_to_val_t type_name1 >>=
      equiv_t_name_val_t type_name2
  ) :: TypeName -> TypeName -> Stateful Bool

equiv_func_types = ( \(InAndOutTs in_t1 out_t1) (InAndOutTs in_t2 out_t2) ->
  equiv_types in_t1 in_t2 >>= \equiv_in_ts ->
  equiv_types out_t1 out_t2 >>= \equiv_out_ts -> 
  return $ equiv_in_ts && equiv_out_ts
  ) :: FuncType -> FuncType -> Stateful Bool

equiv_prod_types = ( \val_ts1 val_ts2 -> case length val_ts1 == length val_ts2 of
  True -> zipWith equiv_types val_ts1 val_ts2==>sequence==>fmap and
  False -> return False
  ) :: [ ValType ] -> [ ValType ] -> Stateful Bool

equiv_t_name_val_t = ( \type_name val_type ->
  t_name_to_val_t type_name >>= equiv_types val_type
  ) :: TypeName -> ValType -> Stateful Bool

t_name_to_val_t = ( \type_name -> 
  type_map_get type_name >>= \case
    TupleType _ fields -> case fields of
      [] -> undefined
      [ field ] -> return $ get_type field
      field1 : field2 : rest ->
        return $ ProdType $ map get_type $ field1 : field2 : rest
    IntType -> return int
    _ -> undefined
  ) :: TypeName -> Stateful ValType
