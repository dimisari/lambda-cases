{-# LANGUAGE LambdaCase #-}

module HaskellTypes.Types where

import Prelude ( String, Show, (++), show, concatMap )

import Helpers ( (-->), (.>)  )
import HaskellTypes.LowLevel ( ValueName )

-- types
newtype TypeName = TN String

data BaseType = TupleType [ ValueType ] | ParenthesisType ValueType | TypeName TypeName

data ValueType = AbstractionTypesAndResultType [ BaseType ] BaseType

data FieldAndType = FT ValueName ValueType

newtype TupleValue = FieldAndTypeList [ FieldAndType ] deriving Show

data TupleType = NameAndTuple TypeName TupleValue

-- Show instances
instance Show TypeName where show = \(TN n) -> n

instance Show BaseType where
  show = \case 
    TupleType vts -> "TupleType " ++ show vts
    ParenthesisType vt -> case vt of
      (AbstractionTypesAndResultType [] (TypeName (TN tn))) -> tn
      _ -> show vt
    TypeName tn -> show tn

instance Show ValueType where
  show = \(AbstractionTypesAndResultType bts bt) ->
    bts-->concatMap (show .> (++ " right_arrow ")) ++ show bt

instance Show FieldAndType where
  show = \(FT vn vt) -> show vn ++ " Type " ++ show vt

instance Show TupleType where
  show = \(NameAndTuple tn tv) -> "\nname:" ++ show tn ++ "\ntuple: " ++ show tv ++ "\n"
