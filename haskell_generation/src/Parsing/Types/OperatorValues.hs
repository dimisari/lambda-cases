module Parsing.Types.OperatorValues where

import Text.Parsec (SourcePos)
import Data.List (intercalate)
import Helpers ((==>), Pos)
import Parsing.Types.LowLevel 

-- All (Types and Show instances):
-- ParenExpr, MathApp, BaseVal,
-- AppDir, FuncAppChain, MultExpr, PlusOrMinus, AddSubExpr,
-- EqualityExpr, InputOpExpr, OpExpr

-- ParenExpr

data ParenExpr =
  ParenExprs OpExpr [ OpExpr ]

instance Show ParenExpr where
  show = \(ParenExprs expr1 exprs) ->
    "(" ++ map show (expr1 : exprs)==>intercalate ", " ++ ")"

-- MathApp

data MathApp =
  NameAndParenExpr2 (Pos ValueName) (Maybe (Pos ParenExpr))

instance Show MathApp where
  show = \(NameAndParenExpr2 val_n maybe_paren_expr) ->
    show val_n ++ case maybe_paren_expr of 
      Just paren_expr -> show paren_expr
      Nothing -> ""

-- BaseVal

data BaseVal =
  Literal (Pos Literal) | ParenExpr (Pos ParenExpr) | MathApp MathApp

instance Show BaseVal where
  show = \case
    ParenExpr paren_expr -> show paren_expr
    Literal lit -> show lit
    MathApp math_app -> show math_app

-- AppDir

data AppDir =
  LeftApp | RightApp

instance Show AppDir where
  show = \case
    LeftApp -> "<=="
    RightApp -> "==>"

-- FuncAppChain

data FuncAppChain =
  ValuesAndDirections BaseVal [ (AppDir, BaseVal) ]

instance Show FuncAppChain where
  show = \(ValuesAndDirections bv ad_bvs) ->
    show bv ++ concatMap ( \(ad, bv) -> show ad ++ show bv ) ad_bvs

-- MultExpr

data MultExpr =
  Factors (Pos FuncAppChain) [ Pos FuncAppChain ]

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
  FirstAndOpTermPairs (Pos MultExpr) [ (PlusOrMinus, Pos MultExpr) ]

instance Show AddSubExpr where
  show = \(FirstAndOpTermPairs term1 pairs) ->
    show term1 ++ concatMap ( \(op, term) -> show op ++ show term ) pairs

-- EqualityExpr

data EqualityExpr =
  EqExpr (Pos AddSubExpr) (Maybe (Pos AddSubExpr))

instance Show EqualityExpr where
  show = \(EqExpr term1 maybe_term2) -> show term1 ++ case maybe_term2 of
    Just term2 -> " = " ++ show term2
    Nothing -> ""

-- InputOpExpr

data InputOpExpr =
  InputEqExpr Input (Pos EqualityExpr)

instance Show InputOpExpr where
  show = \(InputEqExpr input equality_expr) -> show input ++ show equality_expr

-- OpExpr

data OpExpr =
  InputOpExpr InputOpExpr | EqualityExpr (Pos EqualityExpr)

instance Show OpExpr where
  show = \case
    InputOpExpr input_op_expr -> show input_op_expr
    EqualityExpr equality_expr -> show equality_expr
