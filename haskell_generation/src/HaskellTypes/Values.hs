{-# language LambdaCase #-}

module HaskellTypes.Values where

import Data.List
  ( intercalate )
import Helpers
  ( (==>), (.>) )

import HaskellTypes.LowLevel
  ( ValueName, LiteralOrValueName, Abstraction )
import HaskellTypes.Types
  ( ValueType )

-- All: Types, Show instances, error messages

-- Types:
-- ParenthesisValue, BaseValue, ApplicationDirection, OneArgApplications,
-- MultiplicationFactor, Multiplication, SubtractionFactor, Subtraction
-- EqualityFactor, Equality
-- OperatorValue, LambdaOperatorValue, ManyArgsApplication
-- UseFields, SpecificCase, Cases
-- NameTypeAndValue, NameTypeAndValueLists, NTAVOrNTAVLists, NamesTypesAndValues
-- Where, OutputValue, LambdaValue

data ParenthesisValue =
  Parenthesis LambdaValue | Tuple [ LambdaValue ]

data BaseValue =
  ParenthesisValue ParenthesisValue | LiteralOrValueName LiteralOrValueName

data ApplicationDirection =
  LeftApplication | RightApplication

data OneArgApplications = OAA
  ( BaseValue, ApplicationDirection )
  [ ( BaseValue, ApplicationDirection ) ]
  BaseValue

data MultiplicationFactor =
  OneArgAppMF OneArgApplications | BaseValueMF BaseValue

data Multiplication =
  Mul [ MultiplicationFactor ]

data SubtractionFactor =
  MulSF Multiplication | MFSF MultiplicationFactor

data Subtraction =
  Sub SubtractionFactor SubtractionFactor 

data EqualityFactor =
  SubEF Subtraction | SFEF SubtractionFactor

data Equality =
  Equ EqualityFactor EqualityFactor

data OperatorValue =
  Equality Equality | EquF EqualityFactor

data LambdaOperatorValue =
  LOV [ Abstraction ] OperatorValue

data ManyArgsApplication =
  MAA [ LambdaOperatorValue ] ValueName

newtype UseFields =
  UF LambdaValue

data SpecificCase =
  SC LiteralOrValueName LambdaValue 

newtype Cases =
  Cs [ SpecificCase ]

data NameTypeAndValue =
  NTAV ValueName ValueType LambdaValue

data NameTypeAndValueLists =
  NTAVLists [ ValueName ] [ ValueType ] [ LambdaValue ]

data NTAVOrNTAVLists =
  NameTypeAndValue NameTypeAndValue | NameTypeAndValueLists NameTypeAndValueLists

newtype NamesTypesAndValues =
  NTAVs [ NTAVOrNTAVLists ]

data Where =
  Where_ LambdaValue NamesTypesAndValues

data OutputValue =
  ManyArgsApplication ManyArgsApplication | UseFields UseFields | Cases Cases |
  Where Where | OperatorValue OperatorValue

data LambdaValue =
  LV [ Abstraction ] OutputValue

-- Show instances:
-- ParenthesisValue, BaseValue, ApplicationDirection, OneArgApplications,
-- MultiplicationFactor, Multiplication, SubtractionFactor, Subtraction
-- EqualityFactor, Equality
-- OperatorValue, LambdaOperatorValue, ManyArgsApplication
-- UseFields, SpecificCase, Cases
-- NameTypeAndValue, NameTypeAndValueLists, NTAVOrNTAVLists, NamesTypesAndValues
-- Where, OutputValue, LambdaValue

instance Show ParenthesisValue where
  show = \case
    Parenthesis v -> "(" ++ show v ++ ")"
    Tuple vs -> "( " ++ vs==>map show==>intercalate ", " ++ " )"

instance Show BaseValue where
  show = \case
    ParenthesisValue pv -> show pv
    LiteralOrValueName lovn -> show lovn

instance Show ApplicationDirection where
  show = \case
    LeftApplication -> "==>"
    RightApplication -> "<=="

instance Show OneArgApplications where
  show = \(OAA bv_ad bv_ad_s bv) -> case bv_ad_s of
    [] -> error $ one_arg_app_err
    _ ->
      (bv_ad : bv_ad_s)==>concatMap ( \( bv, ad ) -> show bv ++ show ad ) ++
      show bv

instance Show MultiplicationFactor where
  show = \case
    OneArgAppMF oaa -> show oaa
    BaseValueMF bv -> show bv

instance Show Multiplication where
  show = \(Mul mfs) -> case mfs of
      [] -> error less_than_two_mul_err
      [ _ ] -> error less_than_two_mul_err
      mfs -> mfs==>map show==>intercalate " * "

instance Show SubtractionFactor where
  show = \case
    MulSF m -> show m
    MFSF f -> show f

instance Show Subtraction where
  show = \(Sub sf1 sf2) -> show sf1 ++ " - " ++ show sf2

instance Show EqualityFactor where
  show = \case
    SubEF s -> show s
    SFEF f -> show f

instance Show Equality where
  show = \(Equ ef1 ef2) -> show ef1 ++ " = " ++ show ef2

instance Show OperatorValue where
  show = \case
    Equality equ -> show equ
    EquF f -> show f

instance Show LambdaOperatorValue where
  show = \(LOV as nav1) -> show as ++ show nav1

instance Show ManyArgsApplication where
  show = \(MAA maavs vn) ->
    maavs==>map show==>intercalate ", " ++ " *=> " ++ show vn

instance Show UseFields where
  show = \(UF v) -> "use_fields ->\n" ++ show v

instance Show SpecificCase where
  show = \(SC lovn v) -> show lovn ++ " ->\n" ++ show v ++ "\n"

instance Show Cases where
  show = \(Cs scs) -> "\ncases\n\n" ++ scs==>concatMap show

instance Show NameTypeAndValue where
  show = \(NTAV vn vt v) ->
    show vn ++ ": " ++ show vt ++ "\n  = " ++ show v ++ "\n"

instance Show NameTypeAndValueLists where
  show = \(NTAVLists vns vts vs) -> 
    vns==>map show==>intercalate ", " ++ ": " ++
    vts==>map show==>intercalate ", " ++ "\n  = " ++
    vs==>map show==>intercalate ", " ++ "\n"

instance Show NTAVOrNTAVLists where
  show = \case
    NameTypeAndValue ntav -> show ntav
    NameTypeAndValueLists ntavl -> show ntavl

instance Show NamesTypesAndValues where
  show = \(NTAVs ns_ts_and_vs) ->
    "\n" ++ ns_ts_and_vs==>concatMap (show .> (++ "\n"))

instance Show Where where
  show = \(Where_ v ns_ts_and_vs) -> 
    "output\n" ++ show v ++ "where\n" ++ show ns_ts_and_vs 

instance Show OutputValue where
  show = \case
    ManyArgsApplication maa -> show maa
    Cases cs -> show cs
    Where inter_out -> show inter_out
    OperatorValue nav1 -> show nav1

instance Show LambdaValue where
  show = \(LV as nav) -> as==>concatMap (show .> (++ " -> ")) ++ show nav

-- error messages: one_arg_app_err, less_than_two_mul_err
one_arg_app_err =
  "One arg function application should have at least one application direction"
less_than_two_mul_err =
  "Found less than 2 mfs in multiplication"
