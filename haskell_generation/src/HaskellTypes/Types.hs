module HaskellTypes.Types where

import Data.List 
  ( intercalate )

import Helpers
  ( (==>), (.>) )
import HaskellTypes.LowLevel
  ( ValueName )

-- All: Types, Show instances

-- Types:
-- TypeName, ProductType, InputTypeOrTypes, InputTypes, OutputType, FunctionType
-- ValueType
-- FieldNameAndType, TupleTypeDefinition, CaseAndMaybeType, OrTypeDefinition
-- TypeDefinition, FieldsOrCases

newtype TypeName =
  TN String deriving (Eq, Ord)

data ProductType =
  Types ValueType ValueType [ ValueType ]

data InputTypeOrTypes =
  OneInput ValueType | MultipleInputs InputTypes

data InputTypes =
  InTypes ValueType ValueType [ ValueType ]

data OutputType =
  OutputTypeName TypeName | OutputProductType ProductType

data FunctionType =
  InputAndOutput InputTypeOrTypes OutputType

data ValueType =
  FunctionType FunctionType | ProductType ProductType | TypeName TypeName

data FieldNameAndType =
  NameAndType ValueName ValueType 

data TupleTypeDefinition =
  NameAndFields TypeName [ FieldNameAndType ]

data CaseAndMaybeType =
  CaseAndMaybeType ValueName (Maybe ValueType)

data OrTypeDefinition =
  NameAndCases TypeName CaseAndMaybeType CaseAndMaybeType [ CaseAndMaybeType ]

data TypeDefinition =
  TupleTypeDefinition TupleTypeDefinition | OrTypeDefinition OrTypeDefinition

data FieldsOrCases =
  FieldNameAndTypeList [ FieldNameAndType ] |
  CaseAndMaybeTypeList [ CaseAndMaybeType ]

-- Show instances:
-- TypeName, ProductType, InputTypeOrTypes, InputTypes, OutputType, FunctionType
-- ValueType
-- FieldNameAndType, TupleTypeDefinition, CaseAndMaybeType, OrTypeDefinition
-- TypeDefinition

instance Show TypeName where
  show = \(TN name) -> name

instance Show ProductType where
  show = \(Types name1 name2 names) ->
    map show (name1 : name2 : names)==>intercalate " x "

instance Show InputTypeOrTypes where
  show = \case
    OneInput input_type -> show input_type
    MultipleInputs multiple_inputs -> show multiple_inputs

instance Show InputTypes where
  show = \(InTypes input1 input2 inputs) ->
    "(" ++ map show (input1 : input2 : inputs)==>intercalate ", " ++ ")"

instance Show OutputType where
  show = \case
    OutputTypeName name -> show name
    OutputProductType product_type -> show product_type

instance Show FunctionType where
  show = \(InputAndOutput input_type output_type) ->
    show input_type ++ " -> " ++ show output_type

instance Show ValueType where
  show = \case
    FunctionType funcion_type -> show funcion_type
    TypeName name -> show name
    ProductType product_type -> show product_type

instance Show FieldNameAndType where
  show = \(NameAndType field_name field_type) ->
    show field_name ++ ": " ++ show field_type

instance Show TupleTypeDefinition where
  show = \(NameAndFields type_name fields) ->
    "\ntuple_type " ++ show type_name ++
    "\nvalue (" ++ fields==>map show==>intercalate ", "  ++ ")\n"

instance Show CaseAndMaybeType where
  show = \(CaseAndMaybeType case_name maybe_case_type) ->
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
