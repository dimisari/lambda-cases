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
  TupleType [ ValueType ] | ParenthesisType ValueType | TypeName TypeName
  deriving Eq

data ValueType =
  AbsTypesAndResType [ BaseType ] BaseType deriving Eq

data FieldAndType =
  FT { get_fn :: ValueName, get_ft :: ValueType }

newtype TupleTypeValue =
  FieldAndTypeList [ FieldAndType ] 

data TupleType =
  NameAndTupleValue TypeName TupleTypeValue

data CaseAndType =
  CT { get_cn :: ValueName, get_ct :: ValueType }

newtype OrTypeValues =
  CaseAndTypeList [ CaseAndType ] 

data OrType =
  NameAndValues TypeName OrTypeValues

-- Show instances
instance Show TypeName where
  show = \(TN n) -> n

instance Show BaseType where
  show = \case 
    TupleType vts ->
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

instance Show TupleTypeValue where
  show = \(FieldAndTypeList fatl) ->
    "( " ++ concatMap (show .> (++ ", ")) (init fatl) ++ show (last fatl) ++ ")"

instance Show TupleType where
  show = \(NameAndTupleValue tn ttv) ->
    "\ntuple_type" ++ show tn ++ "\nvalue " ++ show ttv ++ "\n"

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
