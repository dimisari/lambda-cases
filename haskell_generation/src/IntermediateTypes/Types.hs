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
  ConsAndInTs TypeName [ ValType ]
  deriving Eq

instance Show TypeApp where
  show = \(ConsAndInTs type_name type_inputs) ->
    show type_name ++ concatMap (show .> (" " ++)) type_inputs

-- ValType

data ValType =
  FuncType FuncType | TypeApp TypeApp | ProdType [ ValType ] | TypeVar Int
  deriving Eq

instance Show ValType where
  show = \case
    FuncType func_type -> show func_type
    TypeApp type_application -> show type_application
    ProdType types -> "(" ++ map show types==>intercalate ", " ++ ")"
    TypeVar int -> ["T1", "T2", "T3", "T4", "T5"] !! (int-1)

-- Helpers: t_name_to_value_t, int, bool

t_name_to_value_t = ( \type_name -> TypeApp $ ConsAndInTs type_name [] )
  :: TypeName -> ValType

int = t_name_to_value_t $ TN "Int"
  :: ValType

bool = t_name_to_value_t $ TN "Bool"
  :: ValType
