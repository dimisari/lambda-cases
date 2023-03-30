module AfterParsing.Types where

import Data.List (intercalate)

import Helpers ((==>), (.>))

import ParsingTypes.LowLevelValues (ValueName)
import ParsingTypes.LowLevelTypes (TypeName)
import ParsingTypes.Types
import ParsingTypes.Values
import ParsingTypes.TypeDefinitions

-- All: Types, Show instances

-- Types: Application, ValueType', TypeApplication', Type Definitions

-- Application: Application, ApplicationTree

data Application =
  ApplicationTrees ApplicationTree ApplicationTree
  deriving Show

data ApplicationTree = 
  Application Application | BaseValueLeaf BaseValue
  deriving Show

-- ValueType':
-- FunctionType', TypeApplication', ValueType' + Show instances

data FunctionType' = 
  InputAndOutputType' ValueType' ValueType'
  deriving Eq

instance Show FunctionType' where
  show = \(InputAndOutputType' in_t out_t) -> (case in_t of
    FunctionType' _ -> "(" ++ show in_t ++ ")"
    _ -> show in_t) ++ " -> " ++ show out_t

data TypeApplication' =
  ConstructorAndInputs' TypeName [ TypeName ]
  deriving Eq

instance Show TypeApplication' where
  show = \(ConstructorAndInputs' type_name input_types) ->
    show type_name ++ concatMap (show .> (" " ++)) input_types

data ValueType' =
  FunctionType' FunctionType' | TypeApplication' TypeApplication' |
  ProductType' [ ValueType' ]
  deriving Eq

instance Show ValueType' where
  show = \case
    FunctionType' func_type -> show func_type
    TypeApplication' type_application -> show type_application
    ProductType' types -> "(" ++ map show types==>intercalate ", " ++ ")"

-- Type Definitions:
-- Field', TupleTypeDefinition', OrTypeCase', OrTypeDefinition', FieldsOrCases

data Field' =
  NameAndType' { get_name :: ValueName, get_type :: ValueType' }
  deriving Show

data TupleTypeDefinition' =
  NameAndFields' TypeApplication' [ Field' ]

data OrTypeCase' =
  NameAndMaybeInputType' ValueName (Maybe ValueType')
  deriving Show

data OrTypeDefinition' =
  NameAndCases' TypeApplication' [ OrTypeCase' ]

data FieldsOrCases =
  FieldList [ Field' ] | OrTypeCaseList [ OrTypeCase' ]
  deriving Show
