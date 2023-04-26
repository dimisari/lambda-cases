module ParsingTypes.Types where

import Data.List (intercalate)
import Helpers ((==>))

-- All (Types and Show instances):
-- TypeName, ProductType, InputTypeOrTypes, ManyTypesInParen, OutputType,
-- FunctionType
-- LeftTInputs, RightTInputs, TypeApplication, ValueType

-- TypeName

newtype TypeName =
  TN String deriving (Eq, Ord)

instance Show TypeName where
  show = \(TN name) -> name

-- ProductType

data ProductType =
  ProductTypes ValueType ValueType [ ValueType ]

instance Show ProductType where
  show = \(ProductTypes t1 t2 ts) -> map show (t1 : t2 : ts)==>intercalate " x "

-- InputTypeOrTypes

data InputTypeOrTypes =
  OneInputType ValueType | MultipleInputTypes ManyTypesInParen

instance Show InputTypeOrTypes where
  show = \case
    OneInputType in_t -> show in_t
    MultipleInputTypes mult_in_ts -> show mult_in_ts

-- ManyTypesInParen

data ManyTypesInParen =
  TypesInParen ValueType ValueType [ ValueType ]

instance Show ManyTypesInParen where
  show = \(TypesInParen t1 t2 ts) ->
    "(" ++ map show (t1 : t2 : ts)==>intercalate ", " ++ ")"

-- OutputType

data OutputType =
  OutputTypeApp TypeApplication | OutputProductType ProductType

instance Show OutputType where
  show = \case
    OutputTypeApp type_app -> show type_app
    OutputProductType prod_type -> show prod_type

-- FunctionType

data FunctionType =
  InAndOutTypes InputTypeOrTypes OutputType

instance Show FunctionType where
  show = \(InAndOutTypes in_ts out_t) -> show in_ts ++ " -> " ++ show out_t

-- LeftTInputs

data LeftTInputs = 
  NoLeftTInputs | OneLeftTInput ValueType | ManyLeftTInputs ManyTypesInParen

instance Show LeftTInputs where
  show = \case
    NoLeftTInputs -> ""
    OneLeftTInput val_t -> show val_t ++ "==>"
    ManyLeftTInputs ts_in_paren -> show ts_in_paren ++ "==>"

-- RightTInputs

data RightTInputs = 
  NoRightTInputs | OneRightTInput ValueType | ManyRightTInputs ManyTypesInParen

instance Show RightTInputs where
  show = \case
    NoRightTInputs -> ""
    OneRightTInput val_t -> "<==" ++ show val_t
    ManyRightTInputs ts_in_paren -> "<=="  ++ show ts_in_paren

-- TypeApplication

data TypeApplication =
  TypeConsAndInputs TypeName LeftTInputs RightTInputs

instance Show TypeApplication where
  show = \(TypeConsAndInputs t_name left_t_inps right_t_inps) ->
    show left_t_inps ++ show t_name ++ show right_t_inps

-- ValueType

data ValueType =
  FunctionType FunctionType | ProductType ProductType |
  TypeApplication TypeApplication 

instance Show ValueType where
  show = \case
    FunctionType func_type -> show func_type
    TypeApplication type_app -> show type_app
    ProductType prod_type -> show prod_type
