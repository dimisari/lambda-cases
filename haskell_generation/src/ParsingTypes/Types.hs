module ParsingTypes.Types where

import Data.List (intercalate)
import Helpers ((==>), (.>))

import ParsingTypes.LowLevelValues (ValueName)
import ParsingTypes.LowLevelTypes (TypeName, ConsAndTypeVars)

-- All: Types, Show instances

-- Types:
-- ProductType, InputTypeOrTypes, InputTypes, OutputType, FunctionType
-- LeftTypeInputs, RightTypeInputs, TypeApplication, ValueType

data ProductType =
  Types ValueType ValueType [ ValueType ]

data InputTypeOrTypes =
  OneInput ValueType | MultipleInputs InputTypes

data InputTypes =
  InTypes ValueType ValueType [ ValueType ]

data OutputType =
  OutputConsAndTypeVars ConsAndTypeVars | OutputProductType ProductType

data FunctionType =
  InputAndOutputTypes InputTypeOrTypes OutputType

data LeftTypeInputs = 
  NoLeftTypeInputs | OneLeftTypeInput ValueType |
  ManyLeftTypeInputs ValueType ValueType [ ValueType ]

data RightTypeInputs = 
  NoRightTypeInputs | OneRightTypeInput ValueType |
  ManyRightTypeInputs ValueType ValueType [ ValueType ]

data TypeApplication =
  ConsAndTypeInputs TypeName LeftTypeInputs RightTypeInputs

data ValueType =
  FunctionType FunctionType | ProductType ProductType |
  ConsAndTypeVars ConsAndTypeVars

-- Show instances:
-- ProductType, InputTypeOrTypes, InputTypes, OutputType, FunctionType
-- LeftTypeInputs, RightTypeInputs, TypeApplication, ValueType

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
    OutputConsAndTypeVars type_application -> show type_application
    OutputProductType product_type -> show product_type

instance Show FunctionType where
  show = \(InputAndOutputTypes input_type output_type) ->
    show input_type ++ " -> " ++ show output_type

instance Show LeftTypeInputs where
  show = \case
    NoLeftTypeInputs -> ""
    OneLeftTypeInput type_ -> show type_ ++ "==>"
    ManyLeftTypeInputs t1 t2 ts ->
      "(" ++ (t1 : t2 : ts)==>map show==>intercalate ", " ++ ")==>"

instance Show RightTypeInputs where
  show = \case
    NoRightTypeInputs -> ""
    OneRightTypeInput type_ -> "<==" ++ show type_
    ManyRightTypeInputs t1 t2 ts ->
      "<==("  ++ (t1 : t2 : ts)==>map show==>intercalate ", " ++ ")"

instance Show TypeApplication where
  show = \(ConsAndTypeInputs type_name left_type_inputs right_type_inputs) ->
    show left_type_inputs ++ show type_name ++ show right_type_inputs

instance Show ValueType where
  show = \case
    FunctionType funcion_type -> show funcion_type
    ConsAndTypeVars name -> show name
    ProductType product_type -> show product_type
