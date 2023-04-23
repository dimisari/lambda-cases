module ParsingTypes.Values where

import Data.List (intercalate)
import Helpers ((==>), (.>))
import ParsingTypes.LowLevel (Literal, ValueName, Input)
import ParsingTypes.Types (ValueType)
import ParsingTypes.OperatorValues (OperatorExpression)

-- All: Types, Show instances

-- Types:
-- LitOrValName, SpecificCase, DefaultCase, Cases, Values
-- Where, CasesOrWhere, InputCasesOrWhere, ValueExpression

data LitOrValName = 
  Literal Literal | ValueName ValueName

data SpecificCase =
  SpecificCase { sc_lovn :: LitOrValName, sc_val_e :: ValueExpression }

data DefaultCase = 
  DefaultCase ValueExpression

data Cases =
  CasesAndMaybeDefault SpecificCase [ SpecificCase ] (Maybe DefaultCase)

data Values =
  NamesTypesAndExpressions [ ValueName ] [ ValueType ] [ ValueExpression ]

data Where =
  ValueExpressionWhereValues ValueExpression [ Values ]

data CasesOrWhere =
  Cases Cases | Where Where

data InputCasesOrWhere =
  InputAndCasesOrWhere Input CasesOrWhere

data ValueExpression =
  InputCasesOrWhere InputCasesOrWhere |
  CasesOrWhere CasesOrWhere |
  OperatorExpression OperatorExpression

-- Show instances:
-- LitOrValName, SpecificCase, DefaultCase, Cases,
-- ValueNamesTypesAndExpressions, Values
-- Where, CasesOrWhere, InputCasesOrWhere, ValueExpression

instance Show LitOrValName where 
  show = \case
    Literal lit -> show lit
    ValueName val_name -> show val_name

instance Show SpecificCase where
  show = \(SpecificCase lit_or_val_name val_expr) ->
    show lit_or_val_name ++ " ->\n" ++ show val_expr ++ "\n"

instance Show DefaultCase where
  show = \(DefaultCase val_expr) -> "... ->\n" ++ show val_expr ++ "\n"

instance Show Cases where
  show = \(CasesAndMaybeDefault case1 cases maybe_def_case) ->
    "\ncases\n\n" ++ concatMap show (case1 : cases) ++ case maybe_def_case of
      Just def_case -> show def_case
      Nothing -> ""

instance Show Values where
  show = \(NamesTypesAndExpressions val_names val_types val_exprs) -> 
    val_names==>map show==>intercalate ", " ++ ": " ++
    val_types==>map show==>intercalate ", " ++ "\n  = " ++
    val_exprs==>map show==>intercalate ", " ++ "\n\n"

instance Show Where where
  show = \(ValueExpressionWhereValues val_expr values) -> 
    "output\n" ++ show val_expr ++ "where\n" ++ show values 

instance Show CasesOrWhere where
  show = \case
    Cases cases -> show cases
    Where where_ -> show where_

instance Show InputCasesOrWhere where
  show = \(InputAndCasesOrWhere input cs_or_where) ->
    show input ++ " -> " ++ show cs_or_where

instance Show ValueExpression where
  show = \case
    InputCasesOrWhere inp_cs_or_where -> show inp_cs_or_where
    CasesOrWhere cs_or_where -> show cs_or_where
    OperatorExpression op_expr -> show op_expr
