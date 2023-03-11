{-# language LambdaCase #-}

module HaskellTypes.Types where

import Data.List 
  ( intercalate )

import Helpers
  ( (==>), (.>) )
import HaskellTypes.LowLevel
  ( ValueName )

-- All: Types, Show instances

-- Types:
-- TypeName, CartesianProduct, Output, MultipleInputs, Input, FunctionType, ValueType,
-- FieldAndType, TupleTypeDef, CaseAndMaybeType, OrTypeDef, TypeDef, FieldsOrCases

newtype TypeName =
  TN String deriving (Eq, Ord)

data CartesianProduct =
  Types ValueType ValueType [ ValueType ]

data Output =
  OutputTypeName TypeName | OutputCartesianProduct CartesianProduct

data MultipleInputs =
  InputTypes ValueType ValueType [ ValueType ]

data Input =
  OneInput ValueType | MultipleInputs MultipleInputs

data FunctionType =
  InputAndOutput Input Output

data ValueType =
  FunctionType FunctionType | CartesianProduct CartesianProduct | TypeName TypeName

data FieldAndType =
  FT { get_field_name :: ValueName, get_field_type :: ValueType }

data TupleTypeDef =
  NameAndValue TypeName [ FieldAndType ]

data CaseAndMaybeType =
  CMT ValueName (Maybe ValueType)

data OrTypeDef =
  NameAndValues TypeName [ CaseAndMaybeType ]

data TypeDef =
  TupleTypeDef TupleTypeDef | OrTypeDef OrTypeDef

data FieldsOrCases =
  FieldAndTypeList [ FieldAndType ] | CaseAndMaybeTypeList [ CaseAndMaybeType ]

-- Show instances:
-- TypeName, CartesianProduct, Output, MultipleInputs, Input, FunctionType, ValueType,
-- FieldAndType, TupleTypeDef CaseAndMaybeType, OrTypeDef, TypeDef
 
instance Show TypeName where
  show = \(TN n) -> n

instance Show CartesianProduct where
  show = \(Types name1 name2 names) ->
    map show (name1 : name2 : names)==>intercalate " x "

instance Show Output where
  show = \case
    OutputTypeName name -> show name
    OutputCartesianProduct cartesian_product -> show cartesian_product

instance Show MultipleInputs where
  show = \(InputTypes input1 input2 inputs) ->
    "(" ++ map show (input1 : input2 : inputs)==>intercalate ", " ++ ")"

instance Show Input where
  show = \case
    OneInput input -> show input
    MultipleInputs multiple_inputs -> show multiple_inputs

instance Show FunctionType where
  show = \(InputAndOutput input output) -> show input ++ " -> " ++ show output

instance Show ValueType where
  show = \case
    FunctionType func_type -> show func_type
    TypeName name -> show name
    CartesianProduct cartesian_product -> show cartesian_product

instance Show FieldAndType where
  show = \(FT vn vt) -> show vn ++ ": " ++ show vt

instance Show TupleTypeDef where
  show = \(NameAndValue tn ttfs) ->
    "\ntuple_type " ++ show tn ++
    "\nvalue (" ++ ttfs==>map show==>intercalate ", "  ++ ")\n"

instance Show CaseAndMaybeType where
  show = \(CMT vn mvt) -> show vn ++ case mvt of 
    Just vt -> "<==(value: " ++ show vt ++ ")"
    Nothing -> ""

instance Show OrTypeDef where
  show = \(NameAndValues tn otvs) ->
    "\nor_type " ++ show tn ++
    "\nvalues " ++ otvs==>map show==>intercalate " | " ++ "\n"

instance Show TypeDef where
  show = \case
    TupleTypeDef ttd -> show ttd
    OrTypeDef otd -> show otd

-- Commented out:
-- newtype ArgName = 
--   AN Char
-- 
-- data TypeConstructorExpr =
--   ArgnamesAndName [ ArgName ] TypeName
