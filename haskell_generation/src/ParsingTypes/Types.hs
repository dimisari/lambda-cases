module ParsingTypes.Types where

import Data.List (intercalate)
import Helpers ((==>))

-- All: Types, Show instances

-- Types:
-- TypeName, ProductType, InputTypeOrTypes, ManyTypesInParenthesis, OutputType,
-- FunctionType
-- LeftTypeInputs, RightTypeInputs, TypeApplication, ValueType

newtype TypeName =
  TN String deriving (Eq, Ord)

data ProductType =
  ProductTypes ValueType ValueType [ ValueType ]

data InputTypeOrTypes =
  OneInputType ValueType | MultipleInputTypes ManyTypesInParenthesis

data ManyTypesInParenthesis =
  TypesInParenthesis ValueType ValueType [ ValueType ]

data OutputType =
  OutputTypeApplication TypeApplication | OutputProductType ProductType

data FunctionType =
  InputAndOutputTypes InputTypeOrTypes OutputType

data LeftTypeInputs = 
  NoLeftTypeInputs | OneLeftTypeInput ValueType |
  ManyLeftTypeInputs ManyTypesInParenthesis

data RightTypeInputs = 
  NoRightTypeInputs | OneRightTypeInput ValueType |
  ManyRightTypeInputs ManyTypesInParenthesis

data TypeApplication =
  TypeConstructorAndInputs TypeName LeftTypeInputs RightTypeInputs

data ValueType =
  FunctionType FunctionType | ProductType ProductType |
  TypeApplication TypeApplication

-- Show instances:
-- TypeName, ProductType, InputTypeOrTypes, ManyTypesInParenthesis, OutputType,
-- FunctionType
-- LeftTypeInputs, RightTypeInputs, TypeApplication, ValueType

instance Show TypeName where
  show = \(TN name) -> name

instance Show ProductType where
  show = \(ProductTypes t1 t2 ts) -> map show (t1 : t2 : ts)==>intercalate " x "

instance Show InputTypeOrTypes where
  show = \case
    OneInputType input_type -> show input_type
    MultipleInputTypes multiple_inputs -> show multiple_inputs

instance Show ManyTypesInParenthesis where
  show = \(TypesInParenthesis t1 t2 ts) ->
    "(" ++ map show (t1 : t2 : ts)==>intercalate ", " ++ ")"

instance Show OutputType where
  show = \case
    OutputTypeApplication type_application -> show type_application
    OutputProductType product_type -> show product_type

instance Show FunctionType where
  show = \(InputAndOutputTypes input_types output_type) ->
    show input_types ++ " -> " ++ show output_type

instance Show LeftTypeInputs where
  show = \case
    NoLeftTypeInputs -> ""
    OneLeftTypeInput value_type -> show value_type ++ "==>"
    ManyLeftTypeInputs many_ts_in_paren -> show many_ts_in_paren ++ "==>"

instance Show RightTypeInputs where
  show = \case
    NoRightTypeInputs -> ""
    OneRightTypeInput value_type -> "<==" ++ show value_type
    ManyRightTypeInputs many_ts_in_paren -> "<=="  ++ show many_ts_in_paren

instance Show TypeApplication where
  show = \(TypeConstructorAndInputs type_name left_type_inputs right_type_inputs) ->
    show left_type_inputs ++ show type_name ++ show right_type_inputs

instance Show ValueType where
  show = \case
    FunctionType funcion_type -> show funcion_type
    TypeApplication type_application -> show type_application
    ProductType product_type -> show product_type
