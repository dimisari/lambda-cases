module HaskellTypes.TypeDefinitions where

import Data.List (intercalate)
import Helpers ((==>), (.>))

import HaskellTypes.LowLevel (ValueName)
import HaskellTypes.LowLevelTypes (TypeName)
import HaskellTypes.Types (ValueType)

-- All: Types, Show instances

-- Types: Field, TupleTypeDefinition, OrTypeCase, OrTypeDefinition, TypeDefinition

data Field =
  NameAndType ValueName ValueType 

data TupleTypeDefinition =
  NameAndFields TypeName [ Field ]

data OrTypeCase =
  OrTypeCase ValueName (Maybe ValueType)

data OrTypeDefinition =
  NameAndCases TypeName OrTypeCase OrTypeCase [ OrTypeCase ]

data TypeDefinition =
  TupleTypeDefinition TupleTypeDefinition | OrTypeDefinition OrTypeDefinition

-- Show instances:
-- Field, TupleTypeDefinition, OrTypeCase, OrTypeDefinition, TypeDefinition

instance Show Field where
  show = \(NameAndType field_name field_type) ->
    show field_name ++ ": " ++ show field_type

instance Show TupleTypeDefinition where
  show = \(NameAndFields type_name fields) ->
    "\ntuple_type " ++ show type_name ++
    "\nvalue (" ++ fields==>map show==>intercalate ", "  ++ ")\n"

instance Show OrTypeCase where
  show = \(OrTypeCase case_name maybe_case_type) ->
    show case_name ++ case maybe_case_type of 
      Just case_type -> "<==(value: " ++ show case_type ++ ")"
      Nothing -> ""

instance Show OrTypeDefinition where
  show = \(NameAndCases type_name case1 case2 cases) ->
    "\nor_type " ++ show type_name ++
    "\nvalues " ++ (case1 : case2 : cases)==>map show==>intercalate " | " ++ "\n"

instance Show TypeDefinition where
  show = \case
    TupleTypeDefinition tuple_type_definition -> show tuple_type_definition   
    OrTypeDefinition or_type_definition -> show or_type_definition
