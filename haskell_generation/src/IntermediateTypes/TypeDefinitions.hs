module IntermediateTypes.TypeDefinitions where

import Helpers ((.>))

import ParsingTypes.LowLevel (ValueName)
import ParsingTypes.Types (TypeName(..))

import IntermediateTypes.Types (ValueType'(..), TypeApplication'(..))

-- All: Types, Show instances, Helpers

-- Types:
-- TypeConstructorAndVariables' Field', TupleTypeDefinition',
-- OrTypeCase', OrTypeDefinition', FieldsOrCases

data TypeConstructorAndVariables' =
  TypeConstructorAndVariables'
    { get_t_cons_name :: TypeName, get_type_vars :: [ (TypeName, String) ] }

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

-- Show instances: TypeConstructorAndVariables'

instance Show TypeConstructorAndVariables' where
  show = \(TypeConstructorAndVariables' type_name type_variables) ->
    show type_name ++ concatMap (snd .> (" " ++)) type_variables

-- Helpers: t_name_to_value_t'

t_name_to_value_t = ( \type_name ->
  TypeApplication' $ TypeConstructorAndInputs' type_name []
  ) :: TypeName -> ValueType'

int_value_t = t_name_to_value_t $ TN "Int"
  :: ValueType'

