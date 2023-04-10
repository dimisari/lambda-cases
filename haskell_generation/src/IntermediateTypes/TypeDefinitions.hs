module IntermediateTypes.TypeDefinitions where

import Helpers ((.>))

import ParsingTypes.LowLevel (ValueName)
import ParsingTypes.Types (TypeName(..))

import IntermediateTypes.Types (ValueType'(..), TypeApplication'(..))

-- All: Types, Show instances, Helpers

-- Types:
-- TypeConsAndVars' Field', TupleTypeDef',
-- OrTypeCase', OrTypeDef', TypeInfo

data TypeConsAndVars' =
  TypeConsAndVars'
    { get_cons :: TypeName, get_type_vars :: [ (TypeName, String) ] }

data Field' =
  NameAndType' { get_name :: ValueName, get_type :: ValueType' }
  deriving Show

data TupleTypeDef' =
  ConsVarsAndFields' TypeConsAndVars' [ Field' ]

data OrTypeCase' =
  NameAndMaybeInT' ValueName (Maybe ValueType')
  deriving Show

data OrTypeDef' =
  ConsVarsAndCases' TypeConsAndVars' [ OrTypeCase' ]

data TypeInfo =
  TupleType Int [ Field' ] | OrType Int [ OrTypeCase' ] | IntType | CharType
  deriving Show

-- Show instances: TypeConsAndVars'

instance Show TypeConsAndVars' where
  show = \(TypeConsAndVars' type_name type_variables) ->
    show type_name ++ concatMap (snd .> (" " ++)) type_variables

-- Helpers: t_name_to_value_t'

t_name_to_value_t = ( \type_name ->
  TypeApplication' $ TypeConsAndInputs' type_name []
  ) :: TypeName -> ValueType'

int = t_name_to_value_t $ TN "Int"
  :: ValueType'

