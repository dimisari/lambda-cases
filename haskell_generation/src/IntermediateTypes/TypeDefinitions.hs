module IntermediateTypes.TypeDefinitions where

import Helpers ((.>))

import ParsingTypes.LowLevel (ValueName)
import ParsingTypes.Types (TypeName(..))

import IntermediateTypes.Types (ValType(..), TypeApp(..))

-- All: Types, Show instances, Helpers

-- Types:
-- TypeConsAndVars' Field', TupleTypeDef',
-- OrTypeCase', OrTypeDef', TypeInfo

data TypeConsAndVars' =
  TypeConsAndVars'
    { get_cons :: TypeName, get_type_vars :: [ (TypeName, String) ] }

data Field' =
  NameAndType' { get_name :: ValueName, get_type :: ValType }
  deriving Show

data TupleTypeDef' =
  ConsVarsAndFields' TypeConsAndVars' [ Field' ]

data OrTypeCase' =
  NameAndMaybeInT' { get_c_name :: ValueName, get_c_t :: (Maybe ValType) }
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
  TypeApp $ TypeConsAndInputs' type_name []
  ) :: TypeName -> ValType

int = t_name_to_value_t $ TN "Int"
  :: ValType
