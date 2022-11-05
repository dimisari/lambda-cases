{-# LANGUAGE LambdaCase #-}

module HaskellTypes.Types where

import Prelude ( String, Show, (++), show, map, concat )

import Helpers ( (-->), (.>)  )
import HaskellTypes.LowLevel ( ValueName )

newtype TypeName = TN String
instance Show TypeName where show = \(TN n) -> n

data BaseType = TupleType [ ValueType ] | ParenthesisType ValueType | TypeName TypeName
instance Show BaseType where
  show = \case 
    TupleType vts -> "TupleType " ++ show vts
    ParenthesisType vt -> case vt of
      (AbstractionTypesAndResultType [] (TypeName (TN "Int"))) -> "Int"
      _ -> show vt
    TypeName tn -> show tn

data ValueType = AbstractionTypesAndResultType [ BaseType ] BaseType
instance Show ValueType where
  show = \(AbstractionTypesAndResultType tpoits tpoit) ->
    tpoits-->map (show .> (++ " right_arrow "))-->concat ++ show tpoit

data FieldAndType = FieldAndType_ ValueName ValueType
instance Show FieldAndType where
  show = \(FieldAndType_ vn vt) -> show vn ++ " Type " ++ show vt

newtype TupleValue = FieldAndTypeList [ FieldAndType ]
  deriving Show

data TupleType = NameAndTuple TypeName TupleValue
instance Show TupleType where
  show = \(NameAndTuple tn tv) -> "\nname:" ++ show tn ++ "\ntuple: " ++ show tv ++ "\n"
