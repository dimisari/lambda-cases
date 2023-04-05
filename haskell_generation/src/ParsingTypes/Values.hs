module ParsingTypes.Values where

import Data.List (intercalate)

import Helpers ((==>), (.>))

import ParsingTypes.LowLevel (Literal, ValueName, Input)
import ParsingTypes.Types (ValueType)
import ParsingTypes.OperatorValues (OperatorExpression)

-- All: Types, Show instances

-- Types:
-- CaseLiteralOrValueName, SpecificCase, DefaultCase, Cases,
-- ValueNamesTypesAndExpressions, Values
-- Where, CasesOrWhere, InputCasesOrWhere, ValueExpression

data CaseLiteralOrValueName = 
  CaseLiteral Literal | CaseValueName ValueName

data SpecificCase =
  SpecificCase CaseLiteralOrValueName ValueExpression 

data DefaultCase = 
  DefaultCase ValueExpression

data Cases =
  OneAndDefault SpecificCase DefaultCase |
  Many SpecificCase SpecificCase [ SpecificCase ] (Maybe DefaultCase)

data ValueNamesTypesAndExpressions =
  NamesTypesAndExpressions [ ValueName ] [ ValueType ] [ ValueExpression ]

newtype Values =
  Values [ ValueNamesTypesAndExpressions ]

data Where =
  ValueExpressionWhereValues ValueExpression Values

data CasesOrWhere =
  Cases Cases | Where Where

data InputCasesOrWhere =
  InputAndCasesOrWhere Input CasesOrWhere

data ValueExpression =
  InputCasesOrWhere InputCasesOrWhere |
  CasesOrWhere CasesOrWhere |
  OperatorExpression OperatorExpression

-- Show instances:
-- CaseLiteralOrValueName, SpecificCase, DefaultCase, Cases,
-- ValueNamesTypesAndExpressions, Values
-- Where, CasesOrWhere, InputCasesOrWhere, ValueExpression

instance Show CaseLiteralOrValueName where 
  show = \case
    CaseLiteral literal -> show literal
    CaseValueName value_name -> show value_name

instance Show SpecificCase where
  show = \(SpecificCase lit_or_val_name value_expression) ->
    show lit_or_val_name ++ " ->\n" ++ show value_expression ++ "\n"

instance Show DefaultCase where
  show = \(DefaultCase value_expression) ->
    "... ->\n" ++ show value_expression ++ "\n"

instance Show Cases where
  show = \cases -> "\ncases\n\n" ++ case cases of 
    OneAndDefault spec_case def_case -> show spec_case ++ show def_case
    Many spec_case1 spec_case2 spec_cases maybe_default_case ->
      (spec_case1 : spec_case2 : spec_cases)==>concatMap show ++
      case maybe_default_case of
        Just default_case -> show default_case
        Nothing -> ""

instance Show ValueNamesTypesAndExpressions where
  show = \(NamesTypesAndExpressions value_names value_types value_exprs) -> 
    value_names==>map show==>intercalate ", " ++ ": " ++
    value_types==>map show==>intercalate ", " ++ "\n  = " ++
    value_exprs==>map show==>intercalate ", " ++ "\n"

instance Show Values where
  show = \(Values values) ->
    "\n" ++ values==>concatMap (show .> (++ "\n"))

instance Show Where where
  show = \(ValueExpressionWhereValues value_expression values) -> 
    "output\n" ++ show value_expression ++ "where\n" ++ show values 

instance Show CasesOrWhere where
  show = \case
    Cases cases -> show cases
    Where where_ -> show where_

instance Show InputCasesOrWhere where
  show = \(InputAndCasesOrWhere input cases_or_where) ->
    show input ++ " -> " ++ show cases_or_where

instance Show ValueExpression where
  show = \case
    InputCasesOrWhere input_cases_or_where -> show input_cases_or_where
    CasesOrWhere cases_or_where -> show cases_or_where
    OperatorExpression operator_expression -> show operator_expression
