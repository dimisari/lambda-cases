{-# language LambdaCase #-}

module HaskellTypes.Types where

import Data.List 
  ( intercalate )

import Helpers
  ( (==>), (.>) )
import HaskellTypes.LowLevel
  ( ValueName )

-- All: Types, Show instances, Eq instances, helpers

-- Types:
-- TypeName, ParenType, BaseType, ValueType, FieldAndType, TupleTypeDef
-- CaseAndMaybeType, OrTypeDef, FieldsOrCases, TypeDef
newtype TypeName =
  TN String deriving ( Eq, Ord )

data ParenType =
  TupleType [ ValueType ] | ParenVT ValueType

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
    TupleType vts -> "( " ++ vts==>map show==>intercalate ", " ++ " )"
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

-- Eq instances:
instance Eq ValueType where
  AbsTypesAndResType [] (ParenType (ParenVT vt1)) == vt2 = vt1 == vt2
  AbsTypesAndResType abs_ts1 bt1 == AbsTypesAndResType abs_ts2 bt2 =
    abs_ts1 == abs_ts2 && bt1 == bt2

instance Eq BaseType where
  ParenType (ParenVT (AbsTypesAndResType [] bt1)) == bt2 = bt1 == bt2
  ParenType pt1 == ParenType pt2 = pt1 == pt2
  TypeName tn1 == TypeName tn2 = tn1 == tn2
  ParenType pt == TypeName tn = False
  TypeName tn == ParenType pt = False

instance Eq ParenType where
  ParenVT (AbsTypesAndResType [] (ParenType pt1)) == pt2 = pt1 == pt2
  ParenVT vt1 == ParenVT vt2 = vt1 == vt2
  TupleType tt1 == TupleType tt2 = tt1 == tt2
  ParenVT vt == TupleType tt = False
  TupleType tt == ParenVT vt = False

-- helpers: vt_shortest_equivalent, vt_bt_are_equivalent
vt_shortest_equivalent = ( \case
  AbsTypesAndResType [] (ParenType (ParenVT vt)) ->
    vt_shortest_equivalent vt
  vt -> vt
  ) :: ValueType -> ValueType

vt_bt_are_equivalent = ( \case
  ( AbsTypesAndResType [] bt1, bt2) -> bt1 == bt2
  ( vt1, ParenType (ParenVT vt2) ) -> vt1 == vt2
  _ -> False
  ) :: ( ValueType, BaseType ) -> Bool
