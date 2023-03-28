module HaskellTypes.LowLevelTypes where

import Data.List (intercalate)

import Helpers ((==>))

-- All: Types, Show instances

-- Types: TypeName, LeftTypeInputs, RightTypeInputs, TypeApplication

newtype TypeName =
  TN String deriving (Eq, Ord)

newtype LeftTypeInputs = 
  LeftTypeInputs [ TypeName ]

newtype RightTypeInputs = 
  RightTypeInputs [ TypeName ]

data TypeApplication =
  NameAndTypeInputs TypeName LeftTypeInputs RightTypeInputs

-- Show instances: TypeName, LeftTypeInputs, RightTypeInputs, TypeApplication

instance Show TypeName where
  show = \(TN name) -> name

instance Show LeftTypeInputs where
  show = \(LeftTypeInputs type_names) ->
    "(" ++ type_names==>map show==>intercalate ", " ++ ")==>"

instance Show RightTypeInputs where
  show = \(RightTypeInputs type_names) ->
    "<==(" ++ type_names==>map show==>intercalate ", " ++ ")"

instance Show TypeApplication where
  show = \(NameAndTypeInputs type_name left_type_inputs right_type_inputs) ->
    show left_type_inputs ++ show type_name ++ show right_type_inputs
