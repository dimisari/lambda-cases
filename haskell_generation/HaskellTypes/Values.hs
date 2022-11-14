{-# language LambdaCase #-}

module HaskellTypes.Values where

import Prelude ( String, Show, (++), show, error, concatMap )

import Helpers ( (-->), (.>) )
import HaskellTypes.LowLevel
  ( ValueName, LiteralOrValueName, ApplicationDirection, Abstractions )
import HaskellTypes.Types ( ValueType )

-- Types
data ParenthesisValue =
  Parenthesis Value | Tuple [ Value ]

data ParenLitOrName =
  ParenthesisValue ParenthesisValue | LiteralOrValueName LiteralOrValueName

data OneArgApplications = 
  OAA ParenLitOrName [ ( ApplicationDirection, ParenLitOrName ) ]

data MultiplicationFactor =
  OneArgAppMF OneArgApplications | ParenLitOrNameMF ParenLitOrName

data Multiplication =
  Mul [ MultiplicationFactor ]

data SubtractionFactor =
  MulSF Multiplication | OAASF OneArgApplications | ParenLitOrNameSF ParenLitOrName

data Subtraction =
  Sub SubtractionFactor SubtractionFactor 

data NoAbstractionsValue1 =
  Subtraction Subtraction | Multiplication Multiplication |
  OneArgApps OneArgApplications | PLON ParenLitOrName 

data ManyArgsArgValue =
  MAAV Abstractions NoAbstractionsValue1

data ManyArgsApplication =
  MAA [ ManyArgsArgValue ] ValueName deriving Show

newtype UseFields =
  UF Value deriving Show

data SpecificCase =
  SC LiteralOrValueName Value 

newtype Cases =
  Cs [ SpecificCase ]

data NameTypeAndValue =
  NTAV ValueName ValueType Value

data NameTypeAndValueLists =
  NTAVLists [ ValueName ] [ ValueType ] [ Value ]

data NTAVOrNTAVLists =
  NameTypeAndValue NameTypeAndValue | NameTypeAndValueLists NameTypeAndValueLists

newtype NamesTypesAndValues =
  NTAVs [ NTAVOrNTAVLists ]

data IntermediatesOutput =
  IntermediatesOutput_ NamesTypesAndValues Value

data NoAbstractionsValue =
  ManyArgsApplication ManyArgsApplication | UseFields UseFields | Cases Cases |
  IntermediatesOutput IntermediatesOutput | NoAbstractionsValue1 NoAbstractionsValue1

data Value =
  Value Abstractions NoAbstractionsValue

-- Show instances
instance Show ParenthesisValue where
  show = \case
    Parenthesis v -> "(" ++ show v ++ ")"
    Tuple vs -> "Tuple " ++ show vs

instance Show ParenLitOrName where
  show = \case
    ParenthesisValue pv -> show pv
    LiteralOrValueName lovn -> show lovn

instance Show OneArgApplications where
  show = \(OAA plon ad_plon_s) -> case ad_plon_s of
    [] ->
      error "one arg function application should have at least one application direction"
    _ ->
      let
      show_ad_plon = ( \( ad, plon ) -> " " ++ show ad ++ " " ++ show plon ++ " " )
        :: ( ApplicationDirection, ParenLitOrName ) -> String
      in
      show plon ++ ad_plon_s-->concatMap show_ad_plon

instance Show MultiplicationFactor where
  show = \case
    OneArgAppMF oaa -> show oaa
    ParenLitOrNameMF plon -> show plon

instance Show Multiplication where
  show = \(Mul mfs) -> case mfs of
      [] -> error "found less than 2 mfs in multiplication"
      [ _ ] -> show (Mul [])
      [ mf1, mf2 ] -> "(" ++ show mf1 ++ " mul " ++ show mf2 ++ ")"
      (mf:mfs) -> "(" ++ show mf ++ " mul " ++ show (Mul mfs) ++ ")"

instance Show SubtractionFactor where
  show = \case
    MulSF m -> show m
    OAASF oaa -> show oaa
    ParenLitOrNameSF plon -> show plon

instance Show Subtraction where
  show = \(Sub sf1 sf2) -> "(" ++ show sf1 ++ " minus " ++ show sf2 ++ ")"

instance Show NoAbstractionsValue1 where
  show = \case
    Subtraction sub -> show sub
    Multiplication mul -> show mul
    OneArgApps oaa -> show oaa
    PLON plon -> show plon

instance Show ManyArgsArgValue where
  show = \(MAAV as nav1) -> show as ++ show nav1

instance Show SpecificCase where
  show = \(SC lovn v) -> 
    "specific case: " ++ show lovn ++ "\n" ++
    "result: " ++ show v ++ "\n"

instance Show Cases where
  show = \(Cs scs) -> "\ncase start\n\n" ++ scs-->concatMap (show .> (++ "\n"))

instance Show NameTypeAndValue where
  show = \(NTAV vn vt v) -> 
    "name: " ++ show vn ++ "\n" ++
    "type: " ++ show vt ++ "\n" ++
    "value: " ++ show v ++ "\n"

instance Show NameTypeAndValueLists where
  show = \(NTAVLists vns vts vs) -> 
    "names: " ++  vns --> concatMap (show .> (++ ", ")) ++ "\n" ++
    "types: " ++ vts --> concatMap (show .> (++ ", ")) ++ "\n" ++
    "values: " ++ vs --> concatMap (show .> (++ ", ")) ++ "\n"

instance Show NTAVOrNTAVLists where
  show = \case
    NameTypeAndValue ntav -> show ntav
    NameTypeAndValueLists ntavl -> show ntavl

instance Show NamesTypesAndValues where
  show = \(NTAVs ns_ts_and_vs) ->
    "\n" ++ ns_ts_and_vs-->concatMap (show .> (++ "\n"))

instance Show IntermediatesOutput where
  show = \(IntermediatesOutput_ ns_ts_and_vs v) -> 
    "intermediates\n" ++ show ns_ts_and_vs ++ "output\n" ++ show v

instance Show NoAbstractionsValue where
  show = \case
    ManyArgsApplication maa -> show maa
    Cases cs -> show cs
    IntermediatesOutput inter_out -> show inter_out
    NoAbstractionsValue1 nav1 -> show nav1

instance Show Value where
  show = \(Value as nav) -> show as ++ show nav
