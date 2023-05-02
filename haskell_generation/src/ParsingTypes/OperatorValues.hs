module ParsingTypes.OperatorValues where

import Data.List (intercalate)
import Helpers ((==>))
import ParsingTypes.LowLevel (Literal, ValueName, Input)

-- All (Types and Show instances):
-- ParenExpr, MathApp, BaseValue,
-- ApplicationDirection, FuncAppChain, MultExpr, PlusOrMinus, AddSubExpr,
-- EqualityExpr, InputOpExpr, OpExpr

-- ParenExpr

data ParenExpr =
  ParenExprs OpExpr [ OpExpr ]

instance Show ParenExpr where
  show = \(ParenExprs expr1 exprs) ->
    "(" ++ map show (expr1 : exprs)==>intercalate ", " ++ ")"

-- MathApp

data MathApp =
  NameAndParenExpr ValueName ParenExpr

instance Show MathApp where
  show = \(NameAndParenExpr val_n paren_expr) -> show val_n ++ show paren_expr

-- BaseValue

data BaseValue =
  Literal Literal | ValueName ValueName | ParenExpr ParenExpr |
  MathApp MathApp

instance Show BaseValue where
  show = \case
    ParenExpr paren_expr -> show paren_expr
    Literal lit -> show lit
    ValueName val_name -> show val_name
    MathApp math_app -> show math_app

-- ApplicationDirection

data ApplicationDirection =
  LeftApplication | RightApplication

instance Show ApplicationDirection where
  show = \case
    LeftApplication -> "<=="
    RightApplication -> "==>"

-- FuncAppChain

data FuncAppChain =
  ValuesAndDirections BaseValue [ (ApplicationDirection, BaseValue) ]

instance Show FuncAppChain where
  show = \(ValuesAndDirections base_val app_dir_base_val_s) ->
    show base_val ++
    concatMap
      ( \(app_dir, base_val) -> show app_dir ++ show base_val ) app_dir_base_val_s

-- MultExpr

data MultExpr =
  Factors FuncAppChain [ FuncAppChain ]

instance Show MultExpr where
  show = \(Factors f1 fs) -> (f1 : fs)==>map show==>intercalate " * "

-- PlusOrMinus

data PlusOrMinus = 
  Plus | Minus

instance Show PlusOrMinus where
  show = \case
    Plus -> " + "
    Minus -> " - "

-- AddSubExpr

data AddSubExpr =
  FirstAndOpTermPairs MultExpr [ (PlusOrMinus, MultExpr) ]

instance Show AddSubExpr where
  show = \(FirstAndOpTermPairs term1 pairs) ->
    show term1 ++ concatMap ( \(op, term) -> show op ++ show term ) pairs

-- EqualityExpr

data EqualityExpr =
  EqExpr AddSubExpr (Maybe AddSubExpr)

instance Show EqualityExpr where
  show = \(EqExpr term1 maybe_term2) -> show term1 ++ case maybe_term2 of
    Just term2 -> " = " ++ show term2
    Nothing -> ""

-- InputOpExpr

data InputOpExpr =
  InputEqExpr Input EqualityExpr

instance Show InputOpExpr where
  show = \(InputEqExpr input equality_expr) -> show input ++ show equality_expr

-- OpExpr

data OpExpr =
  InputOpExpr InputOpExpr | EqualityExpr EqualityExpr

instance Show OpExpr where
  show = \case
    InputOpExpr input_op_expr -> show input_op_expr
    EqualityExpr equality_expr -> show equality_expr
