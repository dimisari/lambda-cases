{-# language LambdaCase #-}

module HaskellTypes.Types where

import Data.List 
  ( intercalate )

import Helpers
  ( (==>), (.>) )
import HaskellTypes.LowLevel
  ( ValueName )

-- All: Types, Show instances, helpers

-- Types:
-- TypeName, ParenType, BaseType, ValueType, FieldAndType, TupleTypeDef
-- CaseAndMaybeType, OrTypeDef, TypeDef, FieldsOrCases
newtype TypeName =
  TN String deriving ( Eq, Ord )

data ParenType =
  TupleType ValueType ValueType [ ValueType ] | ParenVT ValueType

data BaseType =
  TypeName TypeName | ParenType ParenType 

data ValueType =
  AbsTypesAndResType [ BaseType ] BaseType

data FieldAndType =
  FT { get_fn :: ValueName, get_ft :: ValueType }

data TupleTypeDef =
  NameAndValue TypeName [ FieldAndType ]

data CaseAndMaybeType =
  CT ValueName (Maybe ValueType)

data OrTypeDef =
  NameAndValues TypeName [ CaseAndMaybeType ]

data TypeDef =
  TupleTypeDef TupleTypeDef | OrTypeDef OrTypeDef

data FieldsOrCases =
  FieldAndTypeList [ FieldAndType ] | CaseAndMaybeTypeList [ CaseAndMaybeType ]

-- Show instances:
-- TypeName, ParenType, BaseType, ValueType, FieldAndType, TupleTypeDef,
-- CaseAndMaybeType, OrTypeDef, TypeDef
instance Show TypeName where
  show = \(TN n) -> n

instance Show ParenType where
  show = \case 
    TupleType vt1 vt2 vts ->
      "( " ++ (vt1:vt2:vts)==>map show==>intercalate ", " ++ " )"
    ParenVT vt -> vt==> \case
      (AbsTypesAndResType [] (TypeName (TN tn))) -> tn
      _ -> "(" ++ show vt ++ ")"

instance Show BaseType where
  show = \case 
    ParenType pt -> show pt
    TypeName tn -> show tn

instance Show ValueType where
  show = \(AbsTypesAndResType bts bt) ->
    bts==>concatMap (show .> (++ " -> ")) ++ show bt

instance Show FieldAndType where
  show = \(FT vn vt) -> show vn ++ ": " ++ show vt

instance Show TupleTypeDef where
  show = \(NameAndValue tn ttfs) ->
    "\ntuple_type " ++ show tn ++
    "\nvalue ( " ++ ttfs==>map show==>intercalate ", "  ++ " )\n"

instance Show CaseAndMaybeType where
  show = \(CT vn mvt) -> show vn ++ case mvt of 
    Just vt -> "." ++ show vt
    Nothing -> ""

instance Show OrTypeDef where
  show = \(NameAndValues tn otvs) ->
    "\nor_type " ++ show tn ++
    "\nvalues " ++ otvs==>map show==>intercalate " | " ++ "\n"

instance Show TypeDef where
  show = \case
    TupleTypeDef ttd -> show ttd
    OrTypeDef otd -> show otd

-- helpers: vt_to_bt, bt_to_vt
vt_to_bt = \case 
  AbsTypesAndResType [] bt -> bt
  vt -> ParenType $ ParenVT vt
  :: ValueType -> BaseType

bt_to_vt = \case
  ParenType (ParenVT vt) -> vt
  bt -> AbsTypesAndResType [] bt
  :: BaseType -> ValueType
