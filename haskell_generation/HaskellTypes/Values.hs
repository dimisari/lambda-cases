{-# LANGUAGE LambdaCase #-}

module HaskellTypes.Values where

import Prelude ( Show, (++), show, map, concat, error )

import Helpers ( (-->), (.>) )
import HaskellTypes.LowLevel
  ( ValueName, LiteralOrValueName, ApplicationDirection, Abstractions )
import HaskellTypes.Types ( ValueType )

{- 
  All:
  ParenthesisValue, ParenLitOrName, OneArgFunctionApplications,
  MultiplicationFactor, Multiplication, SubtractionFactor, Subtraction,
  NoAbstractionsValue1, ManyArgsAppArgValue, ManyArgsApplication,
  SpecificCase, Cases,
  NameTypeAndValue, NameTypeAndValueLists,
  NTAVOrNTAVLists, NamesTypesAndValues,
  IntermediatesOutput,
  NoAbstractionsValue, Value
-}

-- ParenthesisValue

data ParenthesisValue = Parenthesis Value | Tuple [ Value ]
instance Show ParenthesisValue where
  show = \case
    Parenthesis v -> "(" ++ show v ++ ")"
    Tuple vs -> "Tuple " ++ show vs

-- ParenLitOrName

data ParenLitOrName =
  ParenthesisValue ParenthesisValue | LiteralOrValueName LiteralOrValueName
instance Show ParenLitOrName where
  show = \case
    ParenthesisValue pv -> show pv
    LiteralOrValueName lovn -> show lovn

-- OneArgFunctionApplications

data OneArgFunctionApplications = 
  OneArgFunctionApplications ParenLitOrName [ ( ApplicationDirection, ParenLitOrName ) ]
instance Show OneArgFunctionApplications where
  show = \(OneArgFunctionApplications plon ad_plon_s) -> case ad_plon_s of
    [] -> error "application expression should have at least one application direction"
    _ ->
      let show_ad_plon = ( \( ad, plon ) -> " " ++ show ad ++ " " ++ show plon ++ " " )
      in show plon ++ ad_plon_s-->map show_ad_plon-->concat

-- MultiplicationFactor

data MultiplicationFactor =
  OneArgApplicationsMF OneArgFunctionApplications | ParenLitOrNameMF ParenLitOrName
instance Show MultiplicationFactor where
  show = \case
    OneArgApplicationsMF oafas -> show oafas
    ParenLitOrNameMF plon -> show plon

-- Multiplication

data Multiplication = Multiplication [ MultiplicationFactor ]
instance Show Multiplication where
  show = \(Multiplication mfs) -> case mfs of
      [] -> error "found less than 2 mfs in multiplication"
      [ _ ] -> show (Multiplication [])
      [ mf1, mf2 ] -> "(" ++ show mf1 ++ " mul " ++ show mf2 ++ ")"
      (mf:mfs) -> "(" ++ show mf ++ " mul " ++ show (Multiplication mfs) ++ ")"

-- SubtractionFactor

data SubtractionFactor =
  MultiplicationSF Multiplication | OneArgApplicationsSF OneArgFunctionApplications |
  ParenLitOrNameSF ParenLitOrName
instance Show SubtractionFactor where
  show = \case
    MultiplicationSF m -> show m
    OneArgApplicationsSF oafas -> show oafas
    ParenLitOrNameSF plon -> show plon

-- Subtraction

data Subtraction = Subtraction SubtractionFactor SubtractionFactor 
instance Show Subtraction where
  show = \(Subtraction sf1 sf2) -> "(" ++ show sf1 ++ " minus " ++ show sf2 ++ ")"

-- NoAbstractionsValue1

data NoAbstractionsValue1 =
  Sub Subtraction | Mul Multiplication | OneArgApps OneArgFunctionApplications |
  PLON ParenLitOrName 
instance Show NoAbstractionsValue1 where
  show = \case
    Sub sub -> show sub
    Mul mul -> show mul
    OneArgApps oaas -> show oaas
    PLON plon -> show plon

-- ManyArgsAppArgValue

data ManyArgsAppArgValue = ManyArgsAppArgValue Abstractions NoAbstractionsValue1
instance Show ManyArgsAppArgValue where
  show = \(ManyArgsAppArgValue as nav1) -> show as ++ show nav1

-- ManyArgsApplication

data ManyArgsApplication = ManyArgsApplication [ ManyArgsAppArgValue ] ValueName
  deriving Show

-- SpecificCase

data SpecificCase = SpecificCase LiteralOrValueName Value 
instance Show SpecificCase where
  show = \(SpecificCase lovn v) -> 
    "specific case: " ++ show lovn ++ "\n" ++
    "result: " ++ show v ++ "\n"

-- Cases

newtype Cases = Cases_ [ SpecificCase ]
instance Show Cases where
  show = \(Cases_ scs) -> "\ncase start\n\n" ++ scs-->map (show .> (++ "\n"))-->concat

-- NameTypeAndValue

data NameTypeAndValue = NameTypeAndValue ValueName ValueType Value
instance Show NameTypeAndValue where
  show = \(NameTypeAndValue vn vt v) -> 
    "name: " ++ show vn ++ "\n" ++
    "type: " ++ show vt ++ "\n" ++
    "value: " ++ show v ++ "\n"

-- NameTypeAndValueLists

data NameTypeAndValueLists = NameTypeAndValueLists [ ValueName ] [ ValueType ] [ Value ]
instance Show NameTypeAndValueLists where
  show = \(NameTypeAndValueLists vns vts vs) -> 
    "names: " ++  vns --> map (show .> (++ ", ")) --> concat ++ "\n" ++
    "types: " ++ vts --> map (show .> (++ ", ")) --> concat ++ "\n" ++
    "values: " ++ vs --> map (show .> (++ ", ")) --> concat ++ "\n"

-- NTAVOrNTAVLists

data NTAVOrNTAVLists = NTAV NameTypeAndValue | NTAVLists NameTypeAndValueLists
instance Show NTAVOrNTAVLists where
  show = \case
    NTAV ntav -> show ntav
    NTAVLists ntavl -> show ntavl

-- NamesTypesAndValues

newtype NamesTypesAndValues = NamesTypesAndValues [ NTAVOrNTAVLists ]
instance Show NamesTypesAndValues where
  show = \(NamesTypesAndValues ns_ts_and_vs) ->
    "\n" ++ ns_ts_and_vs-->map (show .> (++ "\n"))-->concat

-- IntermediatesOutput

data IntermediatesOutput = IntermediatesOutput_ NamesTypesAndValues Value
instance Show IntermediatesOutput where
  show = \(IntermediatesOutput_ ns_ts_and_vs v) -> 
    "intermediates\n" ++ show ns_ts_and_vs ++ "output\n" ++ show v

-- NoAbstractionsValue

data NoAbstractionsValue =
  ManyArgsApp ManyArgsApplication | Cases Cases |
  IntermediatesOutput IntermediatesOutput | NoAbstractionsValue1 NoAbstractionsValue1
instance Show NoAbstractionsValue where
  show = \case
    ManyArgsApp maa -> show maa
    Cases cs -> show cs
    IntermediatesOutput io -> show io
    NoAbstractionsValue1 nav1 -> show nav1

-- Value

data Value = Value Abstractions NoAbstractionsValue
instance Show Value where
  show = \(Value as nav) -> show as ++ show nav
