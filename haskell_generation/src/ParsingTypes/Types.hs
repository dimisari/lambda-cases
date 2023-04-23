module ParsingTypes.Types where

import Data.List (intercalate)
import Helpers ((==>))

-- All: Types, Show instances

-- Types:
-- TypeName, ProductType, InputTypeOrTypes, ManyTypesInParen, OutputType,
-- FunctionType
-- LeftTypeInputs, RightTypeInputs, TypeApplication, ValueType

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

data LeftTypeInputs = 
  NoLeftTypeInputs | OneLeftTypeInput ValueType |
  ManyLeftTypeInputs ManyTypesInParen

data RightTypeInputs = 
  NoRightTypeInputs | OneRightTypeInput ValueType |
  ManyRightTypeInputs ManyTypesInParen

data TypeApplication =
  TypeConsAndInputs TypeName LeftTypeInputs RightTypeInputs

data ValueType =
  FunctionType FunctionType | ProductType ProductType |
  TypeApplication TypeApplication 

-- Show instances:
-- TypeName, ProductType, InputTypeOrTypes, ManyTypesInParen, OutputType,
-- FunctionType
-- LeftTypeInputs, RightTypeInputs, TypeApplication, ValueType

instance Show TypeName where
  show = \(TN name) -> name

instance Show ProductType where
  show = \(ProductTypes t1 t2 ts) -> map show (t1 : t2 : ts)==>intercalate " x "

instance Show InputTypeOrTypes where
  show = \case
    OneInputType inp_t -> show inp_t
    MultipleInputTypes mult_inp_ts -> show mult_inp_ts

instance Show ManyTypesInParen where
  show = \(TypesInParen t1 t2 ts) ->
    "(" ++ map show (t1 : t2 : ts)==>intercalate ", " ++ ")"

instance Show OutputType where
  show = \case
    OutputTypeApp type_app -> show type_app
    OutputProductType prod_type -> show prod_type

instance Show FunctionType where
  show = \(InAndOutTypes inp_ts out_t) -> show inp_ts ++ " -> " ++ show out_t

instance Show LeftTypeInputs where
  show = \case
    NoLeftTypeInputs -> ""
    OneLeftTypeInput val_t -> show val_t ++ "==>"
    ManyLeftTypeInputs ts_in_paren -> show ts_in_paren ++ "==>"

instance Show RightTypeInputs where
  show = \case
    NoRightTypeInputs -> ""
    OneRightTypeInput val_t -> "<==" ++ show val_t
    ManyRightTypeInputs ts_in_paren -> "<=="  ++ show ts_in_paren

instance Show TypeApplication where
  show = \(TypeConsAndInputs t_name left_t_inps right_t_inps) ->
    show left_t_inps ++ show t_name ++ show right_t_inps

instance Show ValueType where
  show = \case
    FunctionType func_type -> show func_type
    TypeApplication type_app -> show type_app
    ProductType prod_type -> show prod_type
