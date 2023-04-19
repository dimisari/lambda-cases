module IntermediateTypes.Types where

import Data.List (intercalate)

import Helpers ((==>), (.>))

import ParsingTypes.Types (TypeName(..))

-- All: Types, Show instances

-- Types: FuncType, TypeApp, ValType

data FuncType = 
  InAndOutTs ValType ValType
  deriving Eq

data TypeApp = 
  TypeConsAndInputs' TypeName [ ValType ]
  deriving Eq

data ValType =
  FuncType FuncType | TypeApp TypeApp | ProdType [ ValType ] | TypeVar Int
  deriving Eq

-- Show instances: FuncType, TypeApp, ValType

instance Show FuncType where
  show = \(InAndOutTs in_t out_t) -> (case in_t of
    FuncType _ -> "(" ++ show in_t ++ ")"
    _ -> show in_t) ++ " -> " ++ show out_t

instance Show TypeApp where
  show = \(TypeConsAndInputs' type_name type_inputs) ->
    show type_name ++ concatMap (show .> (" " ++)) type_inputs

instance Show ValType where
  show = \case
    FuncType func_type -> show func_type
    TypeApp type_application -> show type_application
    ProdType types -> "(" ++ map show types==>intercalate ", " ++ ")"
    TypeVar int -> ["T1", "T2", "T3", "T4", "T5"] !! (int-1)

-- Helpers: t_name_to_value_t

t_name_to_value_t = ( \type_name ->
  TypeApp $ TypeConsAndInputs' type_name []
  ) :: TypeName -> ValType

int = t_name_to_value_t $ TN "Int"
  :: ValType

bool = t_name_to_value_t $ TN "Bool"
  :: ValType
