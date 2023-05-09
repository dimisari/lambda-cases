module IntermediateTypes.Types where

import Data.List (intercalate)

import Helpers ((==>), (.>))

import ParsingTypes.Types (TypeName(..))

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
