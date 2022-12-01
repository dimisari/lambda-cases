{-# language LambdaCase #-}

module HaskellTypes.Types where

import Helpers
  ( (==>), (.>) )
import HaskellTypes.LowLevel
  ( ValueName )

-- All: Types, Show instances, helpers

-- Types
newtype TypeName =
  TN String deriving ( Eq, Ord )

data BaseType =
  ParenTupleType [ ValueType ] | ParenthesisType ValueType | TypeName TypeName
  deriving Eq

data ValueType =
  AbsTypesAndResType [ BaseType ] BaseType deriving Eq

data FieldAndType =
  FT { get_fn :: ValueName, get_ft :: ValueType }

data TupleType =
  NameAndValue TypeName [ FieldAndType ]

data CaseAndMaybeType =
  CT { get_cn :: ValueName, get_ct :: Maybe ValueType }

data OrType =
  NameAndValues TypeName [ CaseAndMaybeType ]

data FieldsOrCases =
  FieldAndTypeList [ FieldAndType ] | CaseAndMaybeTypeList [ CaseAndMaybeType ]
  deriving Show

data Type =
  TupleType TupleType | OrType OrType deriving Show

-- Show instances
instance Show TypeName where
  show = \(TN n) -> n

instance Show BaseType where
  show = \case 
    ParenTupleType vts ->
      "( " ++ concatMap (show .> (++ ", ")) (init vts) ++ show (last vts) ++ ")"

    ParenthesisType vt -> vt==> \case
      (AbsTypesAndResType [] (TypeName (TN tn))) -> tn
      _ -> show vt

    TypeName tn -> show tn

instance Show ValueType where
  show = \(AbsTypesAndResType bts bt) ->
    bts==>concatMap (show .> (++ " -> ")) ++ show bt

instance Show FieldAndType where
  show = \(FT vn vt) -> show vn ++ ": " ++ show vt

instance Show TupleType where
  show = \(NameAndValue tn ttv) ->
    "\ntuple_type " ++ show tn ++ "\nvalue ( " ++
    concatMap (show .> (++ ", ")) (init ttv) ++ show (last ttv) ++ " )\n"

instance Show CaseAndMaybeType where
  show = \(CT vn mvt) -> show vn ++ case mvt of 
    Just vt -> "." ++ show vt
    Nothing -> ""

instance Show OrType where
  show = \(NameAndValues tn otvs) ->
    "\nor_type " ++ show tn ++ "\nvalues " ++ 
    concatMap (show .> (++ " | ")) (init otvs) ++ show (last otvs) ++ "\n"

-- helpers
vt_shortest_equivalent = ( \case
  AbsTypesAndResType [] (ParenthesisType vt) -> vt_shortest_equivalent vt
  vt -> vt
  ) :: ValueType -> ValueType

vt_bt_are_equivalent = ( \case
  ( AbsTypesAndResType [] bt1, bt2) -> bt1 == bt2
  ( vt1, ParenthesisType vt2 ) -> vt1 == vt2
  _ -> False
  ) :: ( ValueType, BaseType ) -> Bool
