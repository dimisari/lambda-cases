module Parsing.Types.Values where

import Data.List (intercalate)
import Helpers 
import Parsing.Types.LowLevel (Literal, ValueName, Input)
import Parsing.Types.Types (ValueType)
import Parsing.Types.OperatorValues (OpExpr)

-- All (Types and Show instances):
-- LitOrValName, Case, DefaultCase, CasesExpr, Values
-- Where, CasesOrWhere, InputCasesOrWhere, ValueExpression

-- LitOrValName

data LitOrValName = 
  Literal Literal | ValueName ValueName

instance Show LitOrValName where 
  show = \case
    Literal lit -> show lit
    ValueName val_name -> show val_name

-- Case

data Case =
  Case { case_lovn :: LitOrValName, case_res_expr :: ValueExpression }

instance Show Case where
  show = \(Case lit_or_val_name val_expr) ->
    show lit_or_val_name ++ " ->\n" ++ show val_expr ++ "\n"

-- DefaultCase

data DefaultCase = 
  DefaultCase ValueExpression

instance Show DefaultCase where
  show = \(DefaultCase val_expr) -> "... ->\n" ++ show val_expr ++ "\n"

-- CasesExpr

data CasesExpr =
  CasesAndMaybeDefault Case [ Case ] (Maybe DefaultCase)

instance Show CasesExpr where
  show = \(CasesAndMaybeDefault case1 cases maybe_def_case) ->
    "\ncases\n\n" ++ concatMap show (case1 : cases) ++ case maybe_def_case of
      Just def_case -> show def_case
      Nothing -> ""

-- Values

data Values =
  NamesTypesAndExpressions [ ValueName ] [ ValueType ] [ ValueExpression ]

instance Show Values where
  show = \(NamesTypesAndExpressions val_names val_types val_exprs) -> 
    val_names==>map show==>intercalate ", " ++ ": " ++
    val_types==>map show==>intercalate ", " ++ "\n  = " ++
    val_exprs==>map show==>intercalate ", " ++ "\n\n"

-- Where

data Where =
  ValueExpressionWhereValues ValueExpression [ Values ]

instance Show Where where
  show = \(ValueExpressionWhereValues val_expr values) -> 
    "output\n" ++ show val_expr ++ "where\n" ++ show values 

-- CasesOrWhere

data CasesOrWhere =
  CasesExpr (Pos CasesExpr) | Where Where

instance Show CasesOrWhere where
  show = \case
    CasesExpr cases -> show cases
    Where where_ -> show where_

-- InputCasesOrWhere

data InputCasesOrWhere =
  InputAndCasesOrWhere Input CasesOrWhere

instance Show InputCasesOrWhere where
  show = \(InputAndCasesOrWhere input cs_or_where) ->
    show input ++ " -> " ++ show cs_or_where

-- ValueExpression

data ValueExpression =
  InputCasesOrWhere InputCasesOrWhere |
  CasesOrWhere CasesOrWhere |
  OpExpr OpExpr

instance Show ValueExpression where
  show = \case
    InputCasesOrWhere inp_cs_or_where -> show inp_cs_or_where
    CasesOrWhere cs_or_where -> show cs_or_where
    OpExpr op_expr -> show op_expr
