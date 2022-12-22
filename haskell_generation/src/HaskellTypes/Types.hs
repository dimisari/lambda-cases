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

-- Eq instances:
instance Eq ValueType where
  vt1 == vt2 = case ( vt1, vt2 ) of
    ( AbsTypesAndResType [] (ParenType (ParenVT vt1)), _) -> vt1 == vt2
    ( _, AbsTypesAndResType [] (ParenType (ParenVT vt2)) ) -> vt1 == vt2
    ( AbsTypesAndResType abs_ts1 bt1, AbsTypesAndResType abs_ts2 bt2 ) ->
      abs_ts1 == abs_ts2 && bt1 == bt2

instance Eq BaseType where
  bt1 == bt2 = case ( bt1, bt2 ) of 
    ( ParenType (ParenVT (AbsTypesAndResType [] bt1)), bt2 ) -> bt1 == bt2
    ( bt1, ParenType (ParenVT (AbsTypesAndResType [] bt2)) ) -> bt1 == bt2
    ( ParenType pt1, ParenType pt2 ) -> pt1 == pt2
    ( TypeName tn1, TypeName tn2 ) -> tn1 == tn2
    ( ParenType pt, TypeName tn ) -> False
    ( TypeName tn, ParenType pt ) -> False

instance Eq ParenType where
  pt1 == pt2 = case ( pt1, pt2 ) of 
    ( ParenVT (AbsTypesAndResType [] (ParenType pt1)), pt2 ) -> pt1 == pt2
    ( pt1, ParenVT (AbsTypesAndResType [] (ParenType pt2)) ) -> pt1 == pt2
    ( ParenVT vt1, ParenVT vt2 ) -> vt1 == vt2
    ( TupleType vt1 vt2 vts, TupleType vt1_ vt2_ vts_ ) ->
      vt1 == vt1_ && vt2 == vt2_ && vts == vts_
    ( ParenVT _, TupleType _ _ _ ) -> False
    ( TupleType _ _ _, ParenVT _ ) -> False

-- helpers: vt_to_bt, bt_to_vt
vt_to_bt = \case 
  AbsTypesAndResType [] bt -> bt
  vt -> ParenType $ ParenVT vt
  :: ValueType -> BaseType

bt_to_vt = \case
  ParenType (ParenVT vt) -> vt
  bt -> AbsTypesAndResType [] bt
  :: BaseType -> ValueType
