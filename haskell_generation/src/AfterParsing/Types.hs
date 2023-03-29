module AfterParsing.Types where

import Data.List (intercalate)

import Helpers ((==>))

import ParsingTypes.LowLevel (ValueName)
import ParsingTypes.LowLevelTypes (TypeName)
import ParsingTypes.Types
import ParsingTypes.Values
import ParsingTypes.TypeDefinitions

-- All: Types, Functions

-- Types:
-- Application, ApplicationTree,
-- FunctionType', ValueType',
-- Field', TupleTypeDefinition', OrTypeCase'
-- OrTypeDefinition', TypeDefinition', FieldsOrCases

data Application =
  ApplicationTrees ApplicationTree ApplicationTree
  deriving Show

data ApplicationTree = 
  Application Application | BaseValueLeaf BaseValue
  deriving Show

data FunctionType' = 
  InAndOutType ValueType' ValueType'
  deriving Eq

instance Show FunctionType' where
  show = \(InAndOutType in_t out_t) -> (case in_t of
    FunctionType' _ -> "(" ++ show in_t ++ ")"
    _ -> show in_t) ++ " -> " ++ show out_t

data ValueType' =
  FunctionType' FunctionType' | TypeName' TypeName | ProductType' [ ValueType' ]
  deriving Eq

instance Show ValueType' where
  show = \case
    FunctionType' func_type -> show func_type
    TypeName' type_name -> show type_name
    ProductType' types -> "(" ++ map show types==>intercalate ", " ++ ")"

data Field' =
  FNameAndType { get_name :: ValueName, get_type :: ValueType' }
  deriving Show

data TupleTypeDefinition' =
  TTNameAndFields TypeName [ Field' ]
  deriving Show

data OrTypeCase' =
  NameAndMaybeType ValueName (Maybe ValueType')
  deriving Show

data OrTypeDefinition' =
  ValNameAndCases TypeName [ OrTypeCase' ]
  deriving Show

data TypeDefinition' =
  TupleTypeDefinition' TupleTypeDefinition' | OrTypeDefinition' OrTypeDefinition'
  deriving Show

data FieldsOrCases =
  FieldList [ Field' ] | OrTypeCaseList [ OrTypeCase' ]
  deriving Show
