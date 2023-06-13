module IntermediateTypes.Types where

import Data.List (intercalate)
import qualified Data.Set as S

import Helpers ((==>), (.>))

import Parsing.Types.Types (TypeName(..))

-- All: FuncType, TypeApp, ValType, Helpers

-- FuncType

data FuncType = 
  InAndOutTs ValType ValType
  deriving Eq

instance Show FuncType where
  show = \(InAndOutTs in_t out_t) ->
    let
    in_t_hs = case in_t of
      FuncType _ -> "(" ++ show in_t ++ ")"
      _ -> show in_t
    in
    in_t_hs ++ " -> " ++ show out_t 

-- TypeApp

data TypeApp = 
  ConsAndTIns TypeName [ ValType ]
  deriving Eq

instance Show TypeApp where
  show = \(ConsAndTIns type_name type_inputs) ->
    show type_name ++ concatMap (show .> (" (" ++) .> (++ ")") ) type_inputs

-- ProdType

data ProdType =
  ProdTypes [ ValType ] 
  deriving Eq

instance Show ProdType where
  show = \(ProdTypes types) -> "(" ++ map show types==>intercalate ", " ++ ")"

-- ValType

data ValType =
  FuncType FuncType | TypeApp TypeApp | ProdType ProdType | TypeVar Int
  deriving Eq

instance Show ValType where
  show = \case
    FuncType func_type -> show func_type
    TypeApp type_application -> show type_application
    ProdType prod_type -> show prod_type
    TypeVar int -> "T" ++ show int

-- TypeScheme

data TypeScheme =
  BoundVarsAndT [ Int ] ValType
  deriving (Eq, Show)

-- Helpers: tn_to_val_t, int, bool

tn_to_val_t = ( \type_name -> TypeApp $ ConsAndTIns type_name [] )
  :: TypeName -> ValType

int = tn_to_val_t $ TN "Int"
  :: ValType

bool = tn_to_val_t $ TN "Bool"
  :: ValType

bool_s = BoundVarsAndT [] bool
  :: TypeScheme

int_s = BoundVarsAndT [] int 
  :: TypeScheme
  
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
