module ParsingTypes.TypeDefinitions where

import Data.List (intercalate)
import Helpers ((==>), (.>))

import ParsingTypes.LowLevel (ValueName)
import ParsingTypes.Types (TypeName, ValueType)

-- All: Types, Show instances

-- Types:
-- ManyTypeNamesInParenthesis, LeftTypeVariables, RightTypeVariables,
-- TypeConstructorAndVariables,
-- Field, TupleTypeDefinition, OrTypeCase, OrTypeDefinition, TypeDefinition

data ManyTypeNamesInParenthesis =
  ParenTypeNames TypeName TypeName [ TypeName ]

data LeftTypeVariables = 
  NoLeftTypeVariables | OneLeftTypeVariable TypeName |
  ManyLeftTypeVariables ManyTypeNamesInParenthesis

data RightTypeVariables = 
  NoRightTypeVariables | OneRightTypeVariable TypeName |
  ManyRightTypeVariables ManyTypeNamesInParenthesis

data TypeConstructorAndVariables =
  TypeConstructorAndVariables TypeName LeftTypeVariables RightTypeVariables

data Field =
  NameAndType ValueName ValueType 

data TupleTypeDefinition =
  ConstructorAndFields TypeConstructorAndVariables [ Field ]

data OrTypeCase =
  NameAndMaybeInputType ValueName (Maybe ValueType)

data OrTypeDefinition =
  ConstructorAndCases
    TypeConstructorAndVariables OrTypeCase OrTypeCase [ OrTypeCase ]

data TypeDefinition =
  TupleTypeDefinition TupleTypeDefinition | OrTypeDefinition OrTypeDefinition

-- Show instances:
-- ManyTypeNamesInParenthesis, LeftTypeVariables, RightTypeVariables,
-- TypeConstructorAndVariables,
-- Field, TupleTypeDefinition, OrTypeCase, OrTypeDefinition, TypeDefinition

instance Show ManyTypeNamesInParenthesis where
  show = \(ParenTypeNames tn1 tn2 tns) ->
      "(" ++ (tn1 : tn2 : tns)==>map show==>intercalate ", " ++ ")"

instance Show LeftTypeVariables where
  show = \case
    NoLeftTypeVariables -> ""
    OneLeftTypeVariable type_name -> show type_name ++ "==>"
    ManyLeftTypeVariables many_tns_in_paren -> show many_tns_in_paren ++ "==>"

instance Show RightTypeVariables where
  show = \case
    NoRightTypeVariables -> ""
    OneRightTypeVariable type_name -> "<==" ++ show type_name
    ManyRightTypeVariables many_tns_in_paren -> "<==" ++ show many_tns_in_paren

instance Show TypeConstructorAndVariables where
  show = \(TypeConstructorAndVariables type_name left_type_vars right_type_vars) ->
    show left_type_vars ++ show type_name ++ show right_type_vars

instance Show Field where
  show = \(NameAndType field_name field_type) ->
    show field_name ++ ": " ++ show field_type

instance Show TupleTypeDefinition where
  show = \(ConstructorAndFields type_cons_and_vars fields) ->
    "\ntuple_type " ++ show type_cons_and_vars ++
    "\nvalue (" ++ fields==>map show==>intercalate ", "  ++ ")\n"

instance Show OrTypeCase where
  show = \(NameAndMaybeInputType case_name maybe_case_type) ->
    show case_name ++ case maybe_case_type of 
      Just case_type -> "<==(value: " ++ show case_type ++ ")"
      Nothing -> ""

instance Show OrTypeDefinition where
  show = \(ConstructorAndCases type_cons_and_vars case1 case2 cases) ->
    "\nor_type " ++ show type_cons_and_vars ++
    "\nvalues " ++ (case1 : case2 : cases)==>map show==>intercalate " | " ++ "\n"

instance Show TypeDefinition where
  show = \case
    TupleTypeDefinition tuple_type_definition -> show tuple_type_definition   
    OrTypeDefinition or_type_definition -> show or_type_definition
