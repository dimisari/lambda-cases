{-# LANGUAGE LambdaCase #-}

module HaskellTypes.Values where

import Prelude ( Show, (++), show, error, concatMap )

import Helpers ( (-->), (.>) )
import HaskellTypes.LowLevel
  ( ValueName, LiteralOrValueName, ApplicationDirection, Abstractions )
import HaskellTypes.Types ( ValueType )

--types
data ParenthesisValue = Parenthesis Value | Tuple [ Value ]

data ParenLitOrName =
  ParenthesisValue ParenthesisValue | LiteralOrValueName LiteralOrValueName

data OneArgFunctionApplications = 
  OneArgFunctionApplications ParenLitOrName [ ( ApplicationDirection, ParenLitOrName ) ]

data MultiplicationFactor =
  OneArgApplicationsMF OneArgFunctionApplications | ParenLitOrNameMF ParenLitOrName

data Multiplication = Multiplication [ MultiplicationFactor ]

data SubtractionFactor =
  MultiplicationSF Multiplication | OneArgApplicationsSF OneArgFunctionApplications |
  ParenLitOrNameSF ParenLitOrName

data Subtraction = Subtraction SubtractionFactor SubtractionFactor 

data NoAbstractionsValue1 =
  Sub Subtraction | Mul Multiplication | OneArgApps OneArgFunctionApplications |
  PLON ParenLitOrName 

data ManyArgsAppArgValue = ManyArgsAppArgValue Abstractions NoAbstractionsValue1

data ManyArgsApplication = ManyArgsApplication [ ManyArgsAppArgValue ] ValueName
  deriving Show

data SpecificCase = SpecificCase LiteralOrValueName Value 

newtype Cases = Cs [ SpecificCase ]

data NameTypeAndValue = NameTypeAndValue ValueName ValueType Value

data NameTypeAndValueLists = NameTypeAndValueLists [ ValueName ] [ ValueType ] [ Value ]

data NTAVOrNTAVLists = NTAV NameTypeAndValue | NTAVLists NameTypeAndValueLists

newtype NamesTypesAndValues = NamesTypesAndValues [ NTAVOrNTAVLists ]

data IntermediatesOutput = IntermediatesOutput_ NamesTypesAndValues Value

data NoAbstractionsValue =
  ManyArgsApp ManyArgsApplication | Cases Cases |
  IntermediatesOutput IntermediatesOutput | NoAbstractionsValue1 NoAbstractionsValue1

data Value = Value Abstractions NoAbstractionsValue

-- Show instances
instance Show ParenthesisValue where
  show = \case
    Parenthesis v -> "(" ++ show v ++ ")"
    Tuple vs -> "Tuple " ++ show vs

instance Show ParenLitOrName where
  show = \case
    ParenthesisValue pv -> show pv
    LiteralOrValueName lovn -> show lovn

instance Show OneArgFunctionApplications where
  show = \(OneArgFunctionApplications plon ad_plon_s) -> case ad_plon_s of
    [] ->
      error "one arg function application should have at least one application direction"
    _ ->
      let show_ad_plon = ( \( ad, plon ) -> " " ++ show ad ++ " " ++ show plon ++ " " )
      in show plon ++ ad_plon_s-->concatMap show_ad_plon

instance Show MultiplicationFactor where
  show = \case
    OneArgApplicationsMF oafas -> show oafas
    ParenLitOrNameMF plon -> show plon

instance Show Multiplication where
  show = \(Multiplication mfs) -> case mfs of
      [] -> error "found less than 2 mfs in multiplication"
      [ _ ] -> show (Multiplication [])
      [ mf1, mf2 ] -> "(" ++ show mf1 ++ " mul " ++ show mf2 ++ ")"
      (mf:mfs) -> "(" ++ show mf ++ " mul " ++ show (Multiplication mfs) ++ ")"

instance Show SubtractionFactor where
  show = \case
    MultiplicationSF m -> show m
    OneArgApplicationsSF oafas -> show oafas
    ParenLitOrNameSF plon -> show plon

instance Show Subtraction where
  show = \(Subtraction sf1 sf2) -> "(" ++ show sf1 ++ " minus " ++ show sf2 ++ ")"

instance Show NoAbstractionsValue1 where
  show = \case
    Sub sub -> show sub
    Mul mul -> show mul
    OneArgApps oaas -> show oaas
    PLON plon -> show plon

instance Show ManyArgsAppArgValue where
  show = \(ManyArgsAppArgValue as nav1) -> show as ++ show nav1

instance Show SpecificCase where
  show = \(SpecificCase lovn v) -> 
    "specific case: " ++ show lovn ++ "\n" ++
    "result: " ++ show v ++ "\n"

instance Show Cases where
  show = \(Cs scs) -> "\ncase start\n\n" ++ scs-->concatMap (show .> (++ "\n"))

instance Show NameTypeAndValue where
  show = \(NameTypeAndValue vn vt v) -> 
    "name: " ++ show vn ++ "\n" ++
    "type: " ++ show vt ++ "\n" ++
    "value: " ++ show v ++ "\n"

instance Show NameTypeAndValueLists where
  show = \(NameTypeAndValueLists vns vts vs) -> 
    "names: " ++  vns --> concatMap (show .> (++ ", ")) ++ "\n" ++
    "types: " ++ vts --> concatMap (show .> (++ ", ")) ++ "\n" ++
    "values: " ++ vs --> concatMap (show .> (++ ", ")) ++ "\n"

instance Show NTAVOrNTAVLists where
  show = \case
    NTAV ntav -> show ntav
    NTAVLists ntavl -> show ntavl

instance Show NamesTypesAndValues where
  show = \(NamesTypesAndValues ns_ts_and_vs) ->
    "\n" ++ ns_ts_and_vs-->concatMap (show .> (++ "\n"))

instance Show IntermediatesOutput where
  show = \(IntermediatesOutput_ ns_ts_and_vs v) -> 
    "intermediates\n" ++ show ns_ts_and_vs ++ "output\n" ++ show v

instance Show NoAbstractionsValue where
  show = \case
    ManyArgsApp maa -> show maa
    Cases cs -> show cs
    IntermediatesOutput io -> show io
    NoAbstractionsValue1 nav1 -> show nav1

instance Show Value where
  show = \(Value as nav) -> show as ++ show nav
