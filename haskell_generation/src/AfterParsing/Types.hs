module AfterParsing.Types where

import Data.List (intercalate)

import Helpers ((==>), (.>))

import ParsingTypes.LowLevelValues (ValueName)
import ParsingTypes.LowLevelTypes (TypeName)
import ParsingTypes.Types
import ParsingTypes.Values
import ParsingTypes.TypeDefinitions

-- All: Types, Show instances

-- Types: Application, ValueType', ConsAndTypeVars', Type Definitions

-- Application: Application, ApplicationTree

data Application =
  ApplicationTrees ApplicationTree ApplicationTree
  deriving Show

data ApplicationTree = 
  Application Application | BaseValueLeaf BaseValue
  deriving Show

-- ValueType':
-- FunctionType', ConsAndTypeVars', ValueType' + Show instances

data FunctionType' = 
  InputAndOutputType' ValueType' ValueType'
  deriving Eq

instance Show FunctionType' where
  show = \(InputAndOutputType' in_t out_t) -> (case in_t of
    FunctionType' _ -> "(" ++ show in_t ++ ")"
    _ -> show in_t) ++ " -> " ++ show out_t

data ConsAndTypeVars' =
  ConsAndTVars'
    { get_t_cons_name :: TypeName, get_input_ts :: [ TypeName ] }
  deriving Eq

instance Show ConsAndTypeVars' where
  show = \(ConsAndTVars' type_name input_types) ->
    show type_name ++ concatMap (show .> (" " ++)) input_types

data ValueType' =
  FunctionType' FunctionType' | ConsAndTypeVars' ConsAndTypeVars' |
  ProductType' [ ValueType' ]
  deriving Eq

instance Show ValueType' where
  show = \case
    FunctionType' func_type -> show func_type
    ConsAndTypeVars' type_application -> show type_application
    ProductType' types -> "(" ++ map show types==>intercalate ", " ++ ")"

-- Type Definitions:
-- Field', TupleTypeDefinition', OrTypeCase', OrTypeDefinition', FieldsOrCases

data Field' =
  NameAndType' { get_name :: ValueName, get_type :: ValueType' }
  deriving Show

data TupleTypeDefinition' =
  NameAndFields' ConsAndTypeVars' [ Field' ]

data OrTypeCase' =
  NameAndMaybeInputType' ValueName (Maybe ValueType')
  deriving Show

data OrTypeDefinition' =
  NameAndCases' ConsAndTypeVars' [ OrTypeCase' ]

data FieldsOrCases =
  FieldList [ Field' ] | OrTypeCaseList [ OrTypeCase' ]
  deriving Show
