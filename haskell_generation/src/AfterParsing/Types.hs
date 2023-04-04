module AfterParsing.Types where

import Data.List (intercalate)

import Helpers ((==>), (.>))

import ParsingTypes.LowLevel (ValueName)
import ParsingTypes.Types
import ParsingTypes.OperatorValues (BaseValue)
import ParsingTypes.Values
import ParsingTypes.TypeDefinitions

-- All: Types, Show instances

-- Types: Application, ValueType', TypeConstructorAndVariables', Type Definitions

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

data TypeApplication' = 
  TypeConstructorAndInputs' TypeName [ ValueType' ]
  deriving Eq

data ValueType' =
  FunctionType' FunctionType' | TypeApplication' TypeApplication' |
  ProductType' [ ValueType' ]
  deriving Eq

instance Show FunctionType' where
  show = \(InputAndOutputType' in_t out_t) -> (case in_t of
    FunctionType' _ -> "(" ++ show in_t ++ ")"
    _ -> show in_t) ++ " -> " ++ show out_t

instance Show TypeApplication' where
  show = \(TypeConstructorAndInputs' type_name type_inputs) ->
    show type_name ++ concatMap (show .> (" " ++)) type_inputs

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
  ConstructorAndFields' TypeConstructorAndVariables' [ Field' ]

data OrTypeCase' =
  NameAndMaybeInputType' ValueName (Maybe ValueType')
  deriving Show

data OrTypeDefinition' =
  ConstructorAndCases' TypeConstructorAndVariables' [ OrTypeCase' ]

data FieldsOrCases =
  FieldList [ Field' ] | OrTypeCaseList [ OrTypeCase' ]
  deriving Show

-- Other

data TypeConstructorAndVariables' =
  TypeConstructorAndVariables'
    { get_t_cons_name :: TypeName, get_type_vars :: [ (TypeName, String) ] }

instance Show TypeConstructorAndVariables' where
  show = \(TypeConstructorAndVariables' type_name type_variables) ->
    show type_name ++ concatMap (snd .> (" " ++)) type_variables

-- Helpers: t_name_to_value_t'

t_name_to_value_t = ( \type_name ->
  TypeApplication' $ TypeConstructorAndInputs' type_name []
  ) :: TypeName -> ValueType'

int_value_t = t_name_to_value_t $ TN "Int"
  :: ValueType'

