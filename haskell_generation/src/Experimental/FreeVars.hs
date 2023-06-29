module Experimental.FreeVars where

import qualified Data.Set as S
import Intermediate.Types.Types

-- free vars

class FreeVars a where
  free_vars :: a -> S.Set Int

instance FreeVars ValType where
  free_vars = \case
    FuncType func_type -> free_vars func_type
    TypeApp type_app -> free_vars type_app
    ProdType prod_type -> free_vars prod_type
    TypeVar i -> S.singleton i

instance FreeVars FuncType where
  free_vars = \(InAndOutTs in_t out_t) ->
    S.union (free_vars in_t) (free_vars out_t)

instance FreeVars ProdType where
  free_vars = \(ProdTypes prod_types) ->
    S.unions $ map free_vars prod_types

instance FreeVars TypeApp where
  free_vars = \(ConsAndTIns _ type_inputs) ->
    S.unions $ map free_vars type_inputs

instance FreeVars TypeScheme where
  free_vars = \(BoundVarsAndT bound_vars val_type) ->
    S.difference (free_vars val_type) (S.fromList bound_vars)

-- 

generalize = ( \t -> BoundVarsAndT (S.toList $ free_vars t) t )
  :: ValType -> TypeScheme

instantiate = ( \(BoundVarsAndT _ t) -> t )
  :: TypeScheme -> ValType

no_bound_vars = BoundVarsAndT []
  :: ValType -> TypeScheme 
