module GenerationHelpers.TypeChecking where

import Control.Monad (zipWithM)

import Helpers (Haskell, (==>))

import ParsingTypes.LowLevel (ValueName(..))
import ParsingTypes.Types (TypeName(..))

import IntermediateTypes.Types
import IntermediateTypes.TypeDefinitions (TypeInfo(..), get_type)

import GenerationState.TypesAndOperations (Stateful, type_map_get)

-- All: equiv_types, equiv_func_types, equiv_prod_types,

equiv_types = ( \val_type1 val_type2 -> case (val_type1, val_type2) of
  (TypeApp (ConsAndTIns t_name1 _), TypeApp (ConsAndTIns t_name2 _)) ->
    return $ t_name1 == t_name2
  (ProdType (ProdTypes types1), ProdType (ProdTypes types2)) ->
    equiv_prod_types types1 types2
  (FuncType func_type1, FuncType func_type2) ->
    equiv_func_types func_type1 func_type2
  _ -> return False
  ) :: ValType -> ValType -> Stateful Bool

equiv_func_types = ( \(InAndOutTs in_t1 out_t1) (InAndOutTs in_t2 out_t2) ->
  equiv_types in_t1 in_t2 >>= \equiv_in_ts ->
  equiv_types out_t1 out_t2 >>= \equiv_out_ts -> 
  return $ equiv_in_ts && equiv_out_ts
  ) :: FuncType -> FuncType -> Stateful Bool

equiv_prod_types = ( \val_ts1 val_ts2 -> case length val_ts1 == length val_ts2 of
  True -> zipWithM equiv_types val_ts1 val_ts2==>fmap and
  False -> return False
  ) :: [ ValType ] -> [ ValType ] -> Stateful Bool
