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

-- All: Types, Show instances

-- Types:
-- ParenthesisValue, MathApplication,  BaseValue
-- ApplicationDirection, OneArgApplications
-- MultiplicationFactor, Multiplication, SubtractionFactor, Subtraction
-- EqualityFactor, Equality
-- OperatorValue, LambdaOperatorValue, ManyArgsApplication
-- SpecificCase, Cases
-- NameTypeAndValue, NameTypeAndValueLists, NTAVOrNTAVLists, NamesTypesAndValues
-- Where, OutputValue, LambdaOutputValue

data ParenthesisValue =
  Parenthesis LambdaOutputValue |
  Tuple LambdaOperatorValue LambdaOperatorValue [ LambdaOperatorValue ]

data MathApplication =
  MathApp ValueName LambdaOperatorValue [ LambdaOperatorValue ]

data BaseValue =
  ParenthesisValue ParenthesisValue | LiteralOrValueName LiteralOrValueName |
  MathApplication MathApplication

data ApplicationDirection =
  LeftApplication | RightApplication

-- data LastValue = 
--   BVLV BaseValue | CLV Cases

data OneArgApplications = OAA
  (BaseValue, ApplicationDirection) [ (BaseValue, ApplicationDirection) ] BaseValue

data MultiplicationFactor =
  OneArgAppMF OneArgApplications | BaseValueMF BaseValue

data Multiplication =
  Mul MultiplicationFactor MultiplicationFactor [ MultiplicationFactor ]

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
  MAA LambdaOperatorValue LambdaOperatorValue [ LambdaOperatorValue ] ValueName

data SpecificCase =
  SC LiteralOrValueName LambdaOutputValue 

data Cases =
  Cs SpecificCase SpecificCase [ SpecificCase ]

data NameTypeAndValue =
  NTAV ValueName ValueType LambdaOutputValue

data NameTypeAndValueLists =
  NTAVLists [ ValueName ] [ ValueType ] [ LambdaOutputValue ]

data NTAVOrNTAVLists =
  NameTypeAndValue NameTypeAndValue | NameTypeAndValueLists NameTypeAndValueLists

newtype NamesTypesAndValues =
  NTAVs [ NTAVOrNTAVLists ]

data Where =
  Where_ LambdaOutputValue NamesTypesAndValues

data OutputValue =
  ManyArgsApplication ManyArgsApplication | Cases Cases |
  Where Where | OperatorValue OperatorValue

data LambdaOutputValue =
  LV [ Abstraction ] OutputValue

-- Show instances:
-- ParenthesisValue, MathApplication, BaseValue
-- ApplicationDirection, OneArgApplications
-- MultiplicationFactor, Multiplication, SubtractionFactor, Subtraction
-- EqualityFactor, Equality
-- OperatorValue, LambdaOperatorValue, ManyArgsApplication
-- SpecificCase, Cases
-- NameTypeAndValue, NameTypeAndValueLists, NTAVOrNTAVLists, NamesTypesAndValues
-- Where, OutputValue, LambdaOutputValue

instance Show ParenthesisValue where
  show = \case
    Parenthesis v -> "(" ++ show v ++ ")"
    Tuple v1 v2 vs -> "(" ++ map show (v1 : v2 : vs)==>intercalate ", " ++ ")"

instance Show MathApplication where
  show = \(MathApp vn lov1 lovs) ->
    show vn ++ "(" ++ (lov1 : lovs)==>map show==>intercalate ", " ++ ")"

instance Show BaseValue where
  show = \case
    ParenthesisValue pv -> show pv
    LiteralOrValueName lovn -> show lovn
    MathApplication ma -> show ma

instance Show ApplicationDirection where
  show = \case
    LeftApplication -> "==>"
    RightApplication -> "<=="

instance Show OneArgApplications where
  show = \(OAA bv_ad bv_ad_s bv) ->
    (bv_ad : bv_ad_s)==>concatMap (\(bv, ad) -> show bv ++ show ad) ++ show bv

instance Show MultiplicationFactor where
  show = \case
    OneArgAppMF oaa -> show oaa
    BaseValueMF bv -> show bv

instance Show Multiplication where
  show = \(Mul mf1 mf2 mfs) -> (mf1 : mf2 : mfs)==>map show==>intercalate " * "

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
  show = \(LOV as ov) -> show as ++ show ov

instance Show ManyArgsApplication where
  show = \(MAA lov1 lov2 lovs vn) ->
    map show (lov1 : lov2 : lovs)==>intercalate ", " ++ " *==> " ++ show vn

instance Show SpecificCase where
  show = \(SC lovn v) -> show lovn ++ " ->\n" ++ show v ++ "\n"

instance Show Cases where
  show = \(Cs sc1 sc2 scs) -> "\ncases\n\n" ++ (sc1 : sc2 : scs)==>concatMap show

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
    Where where_ -> show where_
    OperatorValue ov -> show ov

instance Show LambdaOutputValue where
  show = \(LV as outval) -> as==>concatMap (show .> (++ " -> ")) ++ show outval
