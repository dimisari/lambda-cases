module IntermediateTypes.TypeDefinitions where

import Helpers ((.>))

import ParsingTypes.LowLevel (ValueName)
import ParsingTypes.Types (TypeName(..))

import IntermediateTypes.Types (ValType(..))

-- All: TConsAndVars, TTField, TupleTDef, OrTCase, OrTDef, TypeInfo

-- TConsAndVars

data TConsAndVars =
  TConsAndVars { get_cons :: TypeName, get_type_vars :: [ (TypeName, String) ] }

instance Show TConsAndVars where
  show = \(TConsAndVars type_name type_variables) ->
    show type_name ++ concatMap (snd .> (" " ++)) type_variables

-- TTField

data TTField =
  FNameAndType { get_name :: ValueName, get_type :: ValType }
  deriving Show

-- TupleTDef

data TupleTDef =
  TTConsVarsAndFields TConsAndVars [ TTField ]

-- OrTCase

data OrTCase =
  CNameAndMaybeInT { get_c_name :: ValueName, get_c_t :: (Maybe ValType) }
  deriving Show

-- OrTDef

data OrTDef =
  OTConsVarsAndCases TConsAndVars [ OrTCase ]

-- TypeInfo

data TypeInfo =
  TupleType Int [ TTField ] | OrType Int [ OrTCase ] | IntType | CharType
  deriving Show
