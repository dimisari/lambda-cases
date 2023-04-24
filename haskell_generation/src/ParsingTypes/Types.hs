module ParsingTypes.Types where

import Data.List (intercalate)
import Helpers ((==>))

-- All: Types, Show instances

-- Types:
-- TypeName, ProductType, InputTypeOrTypes, ManyTypesInParen, OutputType,
-- FunctionType
-- LeftTInputs, RightTInputs, TypeApplication, ValueType

newtype TypeName =
  TN String deriving (Eq, Ord)

data ProductType =
  ProductTypes ValueType ValueType [ ValueType ]

data InputTypeOrTypes =
  OneInputType ValueType | MultipleInputTypes ManyTypesInParen

data ManyTypesInParen =
  TypesInParen ValueType ValueType [ ValueType ]

data OutputType =
  OutputTypeApp TypeApplication | OutputProductType ProductType

data FunctionType =
  InAndOutTypes InputTypeOrTypes OutputType

data LeftTInputs = 
  NoLeftTInputs | OneLeftTInput ValueType | ManyLeftTInputs ManyTypesInParen

data RightTInputs = 
  NoRightTInputs | OneRightTInput ValueType | ManyRightTInputs ManyTypesInParen

data TypeApplication =
  TypeConsAndInputs TypeName LeftTInputs RightTInputs

data ValueType =
  FunctionType FunctionType | ProductType ProductType |
  TypeApplication TypeApplication 

-- Show instances:
-- TypeName, ProductType, InputTypeOrTypes, ManyTypesInParen, OutputType,
-- FunctionType
-- LeftTInputs, RightTInputs, TypeApplication, ValueType

instance Show TypeName where
  show = \(TN name) -> name

instance Show ProductType where
  show = \(ProductTypes t1 t2 ts) -> map show (t1 : t2 : ts)==>intercalate " x "

instance Show InputTypeOrTypes where
  show = \case
    OneInputType in_t -> show in_t
    MultipleInputTypes mult_in_ts -> show mult_in_ts

instance Show ManyTypesInParen where
  show = \(TypesInParen t1 t2 ts) ->
    "(" ++ map show (t1 : t2 : ts)==>intercalate ", " ++ ")"

instance Show OutputType where
  show = \case
    OutputTypeApp type_app -> show type_app
    OutputProductType prod_type -> show prod_type

instance Show FunctionType where
  show = \(InAndOutTypes in_ts out_t) -> show in_ts ++ " -> " ++ show out_t

instance Show LeftTInputs where
  show = \case
    NoLeftTInputs -> ""
    OneLeftTInput val_t -> show val_t ++ "==>"
    ManyLeftTInputs ts_in_paren -> show ts_in_paren ++ "==>"

instance Show RightTInputs where
  show = \case
    NoRightTInputs -> ""
    OneRightTInput val_t -> "<==" ++ show val_t
    ManyRightTInputs ts_in_paren -> "<=="  ++ show ts_in_paren

instance Show TypeApplication where
  show = \(TypeConsAndInputs t_name left_t_inps right_t_inps) ->
    show left_t_inps ++ show t_name ++ show right_t_inps

instance Show ValueType where
  show = \case
    FunctionType func_type -> show func_type
    TypeApplication type_app -> show type_app
    ProductType prod_type -> show prod_type
