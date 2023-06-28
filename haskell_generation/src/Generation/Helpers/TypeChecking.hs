module Generation.Helpers.TypeChecking where

import Control.Monad (zipWithM)
import Control.Monad.Trans.Except (throwE)

import Helpers ((==>))

import Parsing.Types.Types (TypeName(..))

import Intermediate.Types.Types 

import Generation.State.TypesAndOperations (Stateful, type_map_get)

import Generation.Helpers.ErrorMessages (type_check_err)

-- All:
-- check_equiv_types, equiv_types, equiv_func_types, equiv_prod_types, 
-- equiv_type_apps

check_equiv_types = ( \t1 t2 str ->
  equiv_types t1 t2 >>= \case
    True -> return ()
    False -> throwE $ type_check_err str t1 t2
  ) :: ValType -> ValType -> String -> Stateful ()

equiv_types = ( \vt1 vt2 -> case (vt1, vt2) of
  (TypeApp ta1, TypeApp ta2) -> equiv_type_apps ta1 ta2
  (ProdType pt1, ProdType pt2) -> equiv_prod_types pt1 pt2
  (FuncType ft1, FuncType ft2) -> equiv_func_types ft1 ft2
  _ -> return False
  ) :: ValType -> ValType -> Stateful Bool

equiv_func_types = ( \(InAndOutTs in_t1 out_t1) (InAndOutTs in_t2 out_t2) ->
  equiv_types in_t1 in_t2 >>= \equiv_in_ts ->
  equiv_types out_t1 out_t2 >>= \equiv_out_ts -> 
  return $ equiv_in_ts && equiv_out_ts
  ) :: FuncType -> FuncType -> Stateful Bool

equiv_prod_types = ( \(ProdTypes val_ts1) (ProdTypes val_ts2) ->
  case length val_ts1 == length val_ts2 of
    True -> zipWithM equiv_types val_ts1 val_ts2==>fmap and
    False -> return False
  ) :: ProdType -> ProdType -> Stateful Bool

equiv_type_apps = ( \(ConsAndTIns t_name1 in_ts1) (ConsAndTIns t_name2 in_ts2) ->
  zipWithM equiv_types in_ts1 in_ts2==>fmap and==>fmap (&& (t_name1 == t_name2))
  ) :: TypeApp -> TypeApp -> Stateful Bool
