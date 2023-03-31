module ParsingTypes.LowLevelTypes where

import Data.List (intercalate)

import Helpers ((==>))

-- All: Types, Show instances

-- Types: TypeName, LeftTypeVars, RightTypeVars, ConsAndTypeVars

newtype TypeName =
  TN String deriving (Eq, Ord)

data LeftTypeVars = 
  NoLeftTVars | OneLeftTVar TypeName |
  ManyLeftTVars TypeName TypeName [ TypeName ]

data RightTypeVars = 
  NoRightTVar | OneRightTVar TypeName |
  ManyRightTVars TypeName TypeName [ TypeName ]

data ConsAndTypeVars =
  ConsAndTVars TypeName LeftTypeVars RightTypeVars

-- Show instances: TypeName, LeftTypeVars, RightTypeVars, ConsAndTypeVars

instance Show TypeName where
  show = \(TN name) -> name

instance Show LeftTypeVars where
  show = \case
    NoLeftTVars -> ""
    OneLeftTVar type_name -> show type_name ++ "==>"
    ManyLeftTVars t_name1 t_name2 t_names ->
      "(" ++ (t_name1 : t_name2 : t_names)==>map show==>intercalate ", " ++ ")==>"

instance Show RightTypeVars where
  show = \case
    NoRightTVar -> ""
    OneRightTVar type_name -> "<==" ++ show type_name
    ManyRightTVars t_name1 t_name2 t_names ->
      "<==("  ++ (t_name1 : t_name2 : t_names)==>map show==>intercalate ", " ++ ")"

instance Show ConsAndTypeVars where
  show = \(ConsAndTVars type_name left_type_vars right_type_vars) ->
    show left_type_vars ++ show type_name ++ show right_type_vars
