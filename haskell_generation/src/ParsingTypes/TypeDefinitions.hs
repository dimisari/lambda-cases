module ParsingTypes.TypeDefinitions where

import Data.List (intercalate)
import Helpers ((==>), (.>))

import ParsingTypes.LowLevelValues (ValueName)
import ParsingTypes.LowLevelTypes (TypeName, ConsAndTypeVars)
import ParsingTypes.Types (ValueType)

-- All: Types, Show instances

-- Types: Field, TupleTypeDefinition, OrTypeCase, OrTypeDefinition, TypeDefinition

data Field =
  NameAndType ValueName ValueType 

data TupleTypeDefinition =
  NameAndFields ConsAndTypeVars [ Field ]

data OrTypeCase =
  NameAndMaybeInputType ValueName (Maybe ValueType)

data OrTypeDefinition =
  NameAndCases ConsAndTypeVars OrTypeCase OrTypeCase [ OrTypeCase ]

data TypeDefinition =
  TupleTypeDefinition TupleTypeDefinition | OrTypeDefinition OrTypeDefinition

-- Show instances:
-- Field, TupleTypeDefinition, OrTypeCase, OrTypeDefinition, TypeDefinition

instance Show Field where
  show = \(NameAndType field_name field_type) ->
    show field_name ++ ": " ++ show field_type

instance Show TupleTypeDefinition where
  show = \(NameAndFields type_application fields) ->
    "\ntuple_type " ++ show type_application ++
    "\nvalue (" ++ fields==>map show==>intercalate ", "  ++ ")\n"

instance Show OrTypeCase where
  show = \(NameAndMaybeInputType case_name maybe_case_type) ->
    show case_name ++ case maybe_case_type of 
      Just case_type -> "<==(value: " ++ show case_type ++ ")"
      Nothing -> ""

instance Show OrTypeDefinition where
  show = \(NameAndCases type_application case1 case2 cases) ->
    "\nor_type " ++ show type_application ++
    "\nvalues " ++ (case1 : case2 : cases)==>map show==>intercalate " | " ++ "\n"

instance Show TypeDefinition where
  show = \case
    TupleTypeDefinition tuple_type_definition -> show tuple_type_definition   
    OrTypeDefinition or_type_definition -> show or_type_definition
