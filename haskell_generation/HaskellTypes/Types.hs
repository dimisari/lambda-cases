{-# language LambdaCase #-}

module HaskellTypes.Types where

import Prelude ( String, Bool(..), Show, Eq, Ord, (++), (==), show, concatMap )

import Helpers ( (-->), (.>)  )
import HaskellTypes.LowLevel ( ValueName )

-- Types
newtype TypeName =
  TN String deriving ( Eq, Ord )

data BaseType =
  TupleType [ ValueType ] | ParenthesisType ValueType | TypeName TypeName
  deriving Eq

data ValueType =
  AbsTypesAndResType [ BaseType ] BaseType deriving Eq

data FieldAndType =
  FT { get_vn :: ValueName, get_vt :: ValueType }

newtype TupleTypeValue =
  FieldAndTypeList [ FieldAndType ] deriving Show

data TupleType =
  NameAndTupleValue TypeName TupleTypeValue

-- Show instances
instance Show TypeName where
  show = \(TN n) -> n

instance Show BaseType where
  show = \case 
    TupleType vts -> "TupleType " ++ show vts
    ParenthesisType vt -> case vt of
      (AbsTypesAndResType [] (TypeName (TN tn))) -> tn
      _ -> show vt
    TypeName tn -> show tn

instance Show ValueType where
  show = \(AbsTypesAndResType bts bt) ->
    bts-->concatMap (show .> (++ " right_arrow ")) ++ show bt

instance Show FieldAndType where
  show = \(FT vn vt) -> show vn ++ " Type " ++ show vt

instance Show TupleType where
  show = \(NameAndTupleValue tn tv) ->
    "\nname:" ++ show tn ++ "\ntuple: " ++ show tv ++ "\n"

-- helpers
vt_bt_are_equivalent = ( \case
  ( AbsTypesAndResType [] bt1, bt2) -> bt1 == bt2
  ( vt1, ParenthesisType vt2 ) -> vt1 == vt2
  _ -> False
  ) :: ( ValueType, BaseType ) -> Bool
