{-# language LambdaCase #-}

module HaskellTypes.Values where

import Data.List
  ( intercalate )
import Helpers
  ( (==>), (.>) )

import HaskellTypes.LowLevel
  ( Literal, ValueName, Abstraction )
import HaskellTypes.Types
  ( ValueType )

-- All: Types, Show instances

-- Types:
-- ParenthesisValue, TupleValue, MathApplication, BaseValue
-- ApplicationDirection, FunctionApplicationChain
-- MultiplicationFactor, Multiplication, SubtractionFactor, Subtraction
-- EqualityFactor, Equality
-- OperatorExpression, AbstractionOpExpression, AbsOpOrOpExpression
-- LiteralOrValueName, SpecificCase, DefaultCase, Cases
-- NameTypeAndValue, NameTypeAndValueLists, NTAVOrNTAVLists, NamesTypesAndValues
-- Where, CasesOrWhere, AbstractionCasesOrWhere, ValueExpression

newtype ParenthesisValue =
  Parenthesis AbsOpOrOpExpression 

data TupleValue =
  Values AbsOpOrOpExpression AbsOpOrOpExpression [ AbsOpOrOpExpression ]

data MathApplication =
  MathApp ValueName AbsOpOrOpExpression [ AbsOpOrOpExpression ]

data BaseValue =
  ParenthesisValue ParenthesisValue |
  TupleValue TupleValue |
  Literal Literal |
  ValueName ValueName |
  MathApplication MathApplication

data ApplicationDirection =
  LeftApplication | RightApplication

data FunctionApplicationChain =
  ValuesAndDirections
    (BaseValue, ApplicationDirection)
    [ (BaseValue, ApplicationDirection) ]
    BaseValue

data MultiplicationFactor =
  OneArgAppMF FunctionApplicationChain | BaseValueMF BaseValue

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

data OperatorExpression =
  Equality Equality | EquF EqualityFactor

data AbstractionOpExpression =
  AbstractionAndOpResult Abstraction OperatorExpression 

data AbsOpOrOpExpression =
  AbstractionOpExpression AbstractionOpExpression |
  OperatorExpression OperatorExpression

data LiteralOrValueName = 
  Lit Literal | ValName ValueName

data SpecificCase =
  SC LiteralOrValueName ValueExpression 

data DefaultCase = 
  DC ValueExpression

data Cases =
  OneAndDefault SpecificCase DefaultCase |
  Many SpecificCase SpecificCase [ SpecificCase ] (Maybe DefaultCase)

data NameTypeAndValue =
  NTAV ValueName ValueType ValueExpression

data NameTypeAndValueLists =
  NTAVLists [ ValueName ] [ ValueType ] [ ValueExpression ]

data NTAVOrNTAVLists =
  NameTypeAndValue NameTypeAndValue | NameTypeAndValueLists NameTypeAndValueLists

newtype NamesTypesAndValues =
  NTAVs [ NTAVOrNTAVLists ]

data Where =
  Where_ ValueExpression NamesTypesAndValues

data CasesOrWhere =
  Cases Cases | Where Where

data AbstractionCasesOrWhere =
  AbstractionAndCOWResult Abstraction CasesOrWhere

data ValueExpression =
  AbstractionCasesOrWhere AbstractionCasesOrWhere |
  CasesOrWhere CasesOrWhere |
  AbsOpOrOpExpression AbsOpOrOpExpression

-- Show instances:
-- ParenthesisValue, TupleValue, MathApplication, BaseValue
-- ApplicationDirection, FunctionApplicationChain
-- MultiplicationFactor, Multiplication, SubtractionFactor, Subtraction
-- EqualityFactor, Equality
-- OperatorExpression, AbstractionOpExpression, AbsOpOrOpExpression
-- LiteralOrValueName, SpecificCase, DefaultCase, Cases
-- NameTypeAndValue, NameTypeAndValueLists, NTAVOrNTAVLists, NamesTypesAndValues
-- Where, CasesOrWhere, AbstractionCasesOrWhere, ValueExpression

instance Show ParenthesisValue where
  show = \(Parenthesis value) -> "(" ++ show value ++ ")"

instance Show TupleValue where
  show = \(Values value1 value2 values) ->
    "(" ++ map show (value1 : value2 : values)==>intercalate ", " ++ ")"

instance Show MathApplication where
  show = \(MathApp value_name lov1 lovs) ->
    show value_name ++ "(" ++ (lov1 : lovs)==>map show==>intercalate ", " ++ ")"

instance Show BaseValue where
  show = \case
    ParenthesisValue parenthesis_value -> show parenthesis_value
    TupleValue tuple_value -> show tuple_value
    Literal literal -> show literal
    ValueName value_name -> show value_name
    MathApplication math_application -> show math_application

instance Show ApplicationDirection where
  show = \case
    LeftApplication -> "<=="
    RightApplication -> "==>"

instance Show FunctionApplicationChain where
  show = \(ValuesAndDirections bv_ad bv_ad_s bv) ->
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

instance Show OperatorExpression where
  show = \case
    Equality equ -> show equ
    EquF f -> show f

instance Show AbstractionOpExpression where
  show = \(AbstractionAndOpResult as ov) -> show as ++ show ov

instance Show AbsOpOrOpExpression where
  show = \case
    AbstractionOpExpression abs_op_expr -> show abs_op_expr
    OperatorExpression op_expr -> show op_expr

instance Show LiteralOrValueName where 
  show = \case
    Lit literal -> show literal
    ValName value_name -> show value_name

instance Show SpecificCase where
  show = \(SC lovn value) -> show lovn ++ " ->\n" ++ show value ++ "\n"

instance Show DefaultCase where
  show = \(DC value) -> "... ->\n" ++ show value ++ "\n"

instance Show Cases where
  show = \case
    OneAndDefault sc dc -> "\ncases\n\n" ++ show sc ++ show dc
    Many sc1 sc2 scs mdc ->
      "\ncases\n\n" ++ (sc1 : sc2 : scs)==>concatMap show ++ case mdc of
        Just dc -> show dc
        Nothing -> ""

instance Show NameTypeAndValue where
  show = \(NTAV value_name value_type value) ->
    show value_name ++ ": " ++ show value_type ++ "\n  = " ++ show value ++ "\n"

instance Show NameTypeAndValueLists where
  show = \(NTAVLists value_names value_types values) -> 
    value_names==>map show==>intercalate ", " ++ ": " ++
    value_types==>map show==>intercalate ", " ++ "\n  = " ++
    values==>map show==>intercalate ", " ++ "\n"

instance Show NTAVOrNTAVLists where
  show = \case
    NameTypeAndValue name_type_and_value -> show name_type_and_value
    NameTypeAndValueLists ntav_lists -> show ntav_lists

instance Show NamesTypesAndValues where
  show = \(NTAVs ns_ts_and_vs) ->
    "\n" ++ ns_ts_and_vs==>concatMap (show .> (++ "\n"))

instance Show Where where
  show = \(Where_ value ns_ts_and_vs) -> 
    "output\n" ++ show value ++ "where\n" ++ show ns_ts_and_vs 

instance Show CasesOrWhere where
  show = \case
    Cases cases -> show cases
    Where where_ -> show where_

instance Show AbstractionCasesOrWhere where
  show = \(AbstractionAndCOWResult abs cases_or_where) ->
    show abs ++ " -> " ++ show cases_or_where

instance Show ValueExpression where
  show = \case
    AbstractionCasesOrWhere abs_cases_or_where -> show abs_cases_or_where
    AbsOpOrOpExpression abs_op_expr_or_op_expr -> show abs_op_expr_or_op_expr

-- data BaseValueOrCases = 
--   BaseValue BaseValue | Cases Cases
