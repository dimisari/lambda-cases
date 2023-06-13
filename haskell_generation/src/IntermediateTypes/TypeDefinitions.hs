module IntermediateTypes.TypeDefinitions where

import Helpers ((.>))

import Parsing.Types.LowLevel (ValueName)
import Parsing.Types.Types (TypeName(..))

import IntermediateTypes.Types (ValType(..))

-- All: TNameExpr, TTField, TupleTDef, OrTCase, OrTDef, TypeInfo

-- TNameExpr

data TNameExpr =
  TNameExpr { get_cons :: TypeName, get_type_vars :: [ (TypeName, String) ] }

instance Show TNameExpr where
  show = \(TNameExpr type_name type_variables) ->
    show type_name ++ concatMap (snd .> (" " ++)) type_variables

-- TTField

data TTField =
  FNameAndType { get_name :: ValueName, get_type :: ValType }
  deriving Show

-- TupleTDef

data TupleTDef =
  TTNameExprAndFields TNameExpr [ TTField ]

-- OrTCase

data OrTCase =
  CNameAndMaybeInT { get_c_name :: ValueName, get_c_t :: (Maybe ValType) }
  deriving Show

-- OrTDef

data OrTDef =
  OTNameExprAndCases TNameExpr [ OrTCase ]

-- TypeInfo

data TypeInfo =
  TupleType Int [ TTField ] | OrType Int [ OrTCase ] | IntType | CharType
  deriving Show
