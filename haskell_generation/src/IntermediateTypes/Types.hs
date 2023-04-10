module IntermediateTypes.Types where

import Data.List (intercalate)

import Helpers ((==>), (.>))

import ParsingTypes.Types (TypeName)

-- All: Types, Show instances

-- Types: FunctionType', TypeApplication', ValueType'

data FunctionType' = 
  InputAndOutputType' ValueType' ValueType'
  deriving Eq

data TypeApplication' = 
  TypeConsAndInputs' TypeName [ ValueType' ]
  deriving Eq

data ValueType' =
  FunctionType' FunctionType' | TypeApplication' TypeApplication' |
  ProductType' [ ValueType' ] | TypeVariable' Int
  deriving Eq

-- Show instances: FunctionType', TypeApplication', ValueType'

instance Show FunctionType' where
  show = \(InputAndOutputType' in_t out_t) -> (case in_t of
    FunctionType' _ -> "(" ++ show in_t ++ ")"
    _ -> show in_t) ++ " -> " ++ show out_t

instance Show TypeApplication' where
  show = \(TypeConsAndInputs' type_name type_inputs) ->
    show type_name ++ concatMap (show .> (" " ++)) type_inputs

instance Show ValueType' where
  show = \case
    FunctionType' func_type -> show func_type
    TypeApplication' type_application -> show type_application
    ProductType' types -> "(" ++ map show types==>intercalate ", " ++ ")"
    TypeVariable' int -> ["T1", "T2", "T3", "T4", "T5"] !! (int-1)
