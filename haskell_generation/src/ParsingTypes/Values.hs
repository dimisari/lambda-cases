module ParsingTypes.Values where

import Data.List (intercalate)

import Helpers ((==>), (.>))

import ParsingTypes.LowLevel (Literal, ValueName, Input)
import ParsingTypes.Types (ValueType)
import ParsingTypes.OperatorValues (InputOpExprOrOpExpr)

-- All: Types, Show instances

-- Types:
-- LiteralOrValueName, SpecificCase, DefaultCase, Cases
-- NameTypeAndValue, NameTypeAndValueLists, NTAVOrNTAVLists, NamesTypesAndValues
-- Where, CasesOrWhere, InputCasesOrWhere, ValueExpression

data LiteralOrValueName = 
  Lit Literal | ValName ValueName

data SpecificCase =
  SpecificCase LiteralOrValueName ValueExpression 

data DefaultCase = 
  DefaultCase ValueExpression

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
  ValueWhereNTAVs ValueExpression NamesTypesAndValues

data CasesOrWhere =
  Cases Cases | Where Where

data InputCasesOrWhere =
  InputAndCOWResult Input CasesOrWhere

data ValueExpression =
  InputCasesOrWhere InputCasesOrWhere |
  CasesOrWhere CasesOrWhere |
  InputOpExprOrOpExpr InputOpExprOrOpExpr

-- Show instances:
-- LiteralOrValueName, SpecificCase, DefaultCase, Cases
-- NameTypeAndValue, NameTypeAndValueLists, NTAVOrNTAVLists, NamesTypesAndValues
-- Where, CasesOrWhere, InputCasesOrWhere, ValueExpression

instance Show LiteralOrValueName where 
  show = \case
    Lit literal -> show literal
    ValName value_name -> show value_name

instance Show SpecificCase where
  show = \(SpecificCase lit_or_val_name value_expr) ->
    show lit_or_val_name ++ " ->\n" ++ show value_expr ++ "\n"

instance Show DefaultCase where
  show = \(DefaultCase value_expr) -> "... ->\n" ++ show value_expr ++ "\n"

instance Show Cases where
  show = \cases -> "\ncases\n\n" ++ case cases of 
    OneAndDefault spec_case def_case -> show spec_case ++ show def_case
    Many spec_case1 spec_case2 spec_cases maybe_default_case ->
      (spec_case1 : spec_case2 : spec_cases)==>concatMap show ++
      case maybe_default_case of
        Just default_case -> show default_case
        Nothing -> ""

instance Show NameTypeAndValue where
  show = \(NTAV value_name value_type value_expr) ->
    show value_name ++ ": " ++ show value_type ++
    "\n  = " ++ show value_expr ++ "\n"

instance Show NameTypeAndValueLists where
  show = \(NTAVLists value_names value_types value_exprs) -> 
    value_names==>map show==>intercalate ", " ++ ": " ++
    value_types==>map show==>intercalate ", " ++ "\n  = " ++
    value_exprs==>map show==>intercalate ", " ++ "\n"

instance Show NTAVOrNTAVLists where
  show = \case
    NameTypeAndValue name_type_and_value -> show name_type_and_value
    NameTypeAndValueLists ntav_lists -> show ntav_lists

instance Show NamesTypesAndValues where
  show = \(NTAVs ns_ts_and_vs) ->
    "\n" ++ ns_ts_and_vs==>concatMap (show .> (++ "\n"))

instance Show Where where
  show = \(ValueWhereNTAVs value_expr ns_ts_and_vs) -> 
    "output\n" ++ show value_expr ++ "where\n" ++ show ns_ts_and_vs 

instance Show CasesOrWhere where
  show = \case
    Cases cases -> show cases
    Where where_ -> show where_

instance Show InputCasesOrWhere where
  show = \(InputAndCOWResult abs cases_or_where) ->
    show abs ++ " -> " ++ show cases_or_where

instance Show ValueExpression where
  show = \case
    InputCasesOrWhere abs_cases_or_where -> show abs_cases_or_where
    InputOpExprOrOpExpr abs_op_expr_or_op_expr -> show abs_op_expr_or_op_expr
