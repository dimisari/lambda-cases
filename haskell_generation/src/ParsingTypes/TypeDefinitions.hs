module ParsingTypes.TypeDefinitions where

import Data.List (intercalate)
import Helpers ((==>))
import ParsingTypes.LowLevel (ValueName)
import ParsingTypes.Types (TypeName, ValueType)

-- All: Types, Show instances

-- Types:
-- ManyTNamesInParen, LeftTypeVars, RightTypeVars,
-- TypeNameExpr,
-- Field, TupleTypeDef, OrTypeCase, OrTypeDef, TypeDefinition

-- ManyTNamesInParen

data ManyTNamesInParen =
  ParenTypeNames TypeName TypeName [ TypeName ]

instance Show ManyTNamesInParen where
  show = \(ParenTypeNames tn1 tn2 tns) ->
      "(" ++ (tn1 : tn2 : tns)==>map show==>intercalate ", " ++ ")"

-- LeftTypeVars

data LeftTypeVars = 
  NoLeftTypeVars | OneLeftTypeVar TypeName | ManyLeftTypeVars ManyTNamesInParen

instance Show LeftTypeVars where
  show = \case
    NoLeftTypeVars -> ""
    OneLeftTypeVar type_name -> show type_name ++ "==>"
    ManyLeftTypeVars t_names_in_paren -> show t_names_in_paren ++ "==>"

-- RightTypeVars

data RightTypeVars = 
  NoRightTypeVars | OneRightTypeVar TypeName | ManyRightTypeVars ManyTNamesInParen

instance Show RightTypeVars where
  show = \case
    NoRightTypeVars -> ""
    OneRightTypeVar type_name -> "<==" ++ show type_name
    ManyRightTypeVars t_names_in_paren -> "<==" ++ show t_names_in_paren

-- TypeNameExpr

data TypeNameExpr =
  TypeNameExpr TypeName LeftTypeVars RightTypeVars

instance Show TypeNameExpr where
  show = \(TypeNameExpr type_name left_type_vars right_type_vars) ->
    show left_type_vars ++ show type_name ++ show right_type_vars

-- Field

data Field =
  NameAndType ValueName ValueType 

instance Show Field where
  show = \(NameAndType field_name field_type) ->
    show field_name ++ ": " ++ show field_type

-- TupleTypeDef

data TupleTypeDef =
  NameExprAndFields TypeNameExpr [ Field ]

instance Show TupleTypeDef where
  show = \(NameExprAndFields type_cons_and_vars fields) ->
    "\ntuple_type " ++ show type_cons_and_vars ++
    "\nvalue (" ++ fields==>map show==>intercalate ", "  ++ ")\n"

-- OrTypeCase

data OrTypeCase =
  NameAndMaybeInT ValueName (Maybe ValueType)

instance Show OrTypeCase where
  show = \(NameAndMaybeInT case_name maybe_case_type) ->
    show case_name ++ case maybe_case_type of 
      Just case_type -> "<==(value: " ++ show case_type ++ ")"
      Nothing -> ""

-- OrTypeDef

data OrTypeDef =
  NameExprAndCases TypeNameExpr OrTypeCase OrTypeCase [ OrTypeCase ]

instance Show OrTypeDef where
  show = \(NameExprAndCases type_cons_and_vars case1 case2 cases) ->
    "\nor_type " ++ show type_cons_and_vars ++
    "\nvalues " ++ (case1 : case2 : cases)==>map show==>intercalate " | " ++ "\n"

-- TypeDefinition

data TypeDefinition =
  TupleTypeDef TupleTypeDef | OrTypeDef OrTypeDef

instance Show TypeDefinition where
  show = \case
    TupleTypeDef tuple_type_def -> show tuple_type_def   
    OrTypeDef or_type_def -> show or_type_def

