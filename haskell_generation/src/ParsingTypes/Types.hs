module ParsingTypes.Types where

import Data.List (intercalate)
import Helpers ((==>), (.>))

import ParsingTypes.LowLevel (ValueName)
import ParsingTypes.LowLevelTypes (TypeName)

-- All: Types, Show instances

-- Types:
-- ProductType, InputTypeOrTypes, InputTypes, OutputType, FunctionType, ValueType

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

-- Show instances:
-- ProductType, InputTypeOrTypes, InputTypes, OutputType, FunctionType, ValueType

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
