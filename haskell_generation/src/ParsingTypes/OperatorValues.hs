module ParsingTypes.OperatorValues where

import Data.List (intercalate)
import Helpers ((==>))
import ParsingTypes.LowLevel (Literal, ValueName, Input)

-- All: Types, Show instances

-- Types:
-- Parenthesis, Tuple, MathApplication, BaseValue,
-- ApplicationDirection, FuncAppChain, MultExpr, PlusOrMinus, AddSubExpr,
-- Equality, PureOpExpr, InputOpExpr, OperatorExpression

newtype Parenthesis =
  InnerExpression OperatorExpression 

data Tuple =
  TupleExpressions OperatorExpression OperatorExpression [ OperatorExpression ]

data MathApplication =
  NameAndInputExprs ValueName OperatorExpression [ OperatorExpression ]

data BaseValue =
  Literal Literal | ValueName ValueName | Parenthesis Parenthesis |
  Tuple Tuple | MathApplication MathApplication

data ApplicationDirection =
  LeftApplication | RightApplication

data FuncAppChain =
  ValuesAndDirections BaseValue [ (ApplicationDirection, BaseValue) ]

data MultExpr =
  Factors FuncAppChain [ FuncAppChain ]

data PlusOrMinus = 
  Plus | Minus

data AddSubExpr =
  FirstAndOpTermPairs MultExpr [ (PlusOrMinus, MultExpr) ]

data Equality =
  EqualityTerms AddSubExpr AddSubExpr

data PureOpExpr =
  Equality Equality | AddSubExpr AddSubExpr

data InputOpExpr =
  InputAndPureOpExpr Input PureOpExpr 

data OperatorExpression =
  InputOpExpr InputOpExpr | PureOpExpr PureOpExpr

-- Show instances:
-- Parenthesis, Tuple, MathApplication, BaseValue,
-- ApplicationDirection, FuncAppChain, MultExpr, PlusOrMinus, AddSubExpr,
-- Equality, PureOpExpr, InputOpExpr, OperatorExpression

instance Show Parenthesis where
  show = \(InnerExpression expr) -> "(" ++ show expr ++ ")"

instance Show Tuple where
  show = \(TupleExpressions expr1 expr2 exprs) ->
    "(" ++ map show (expr1 : expr2 : exprs)==>intercalate ", " ++ ")"

instance Show MathApplication where
  show = \(NameAndInputExprs val_name expr1 exprs) ->
    show val_name ++ "(" ++ (expr1 : exprs)==>map show==>intercalate ", " ++ ")"

instance Show BaseValue where
  show = \case
    Parenthesis paren_val -> show paren_val
    Tuple tuple -> show tuple
    Literal lit -> show lit
    ValueName val_name -> show val_name
    MathApplication math_app -> show math_app

instance Show ApplicationDirection where
  show = \case
    LeftApplication -> "<=="
    RightApplication -> "==>"

instance Show FuncAppChain where
  show = \(ValuesAndDirections base_val app_dir_base_val_s) ->
    show base_val ++
    concatMap
      ( \(app_dir, base_val) -> show app_dir ++ show base_val ) app_dir_base_val_s

instance Show MultExpr where
  show = \(Factors f1 fs) -> (f1 : fs)==>map show==>intercalate " * "

instance Show PlusOrMinus where
  show = \case
    Plus -> " + "
    Minus -> " - "

instance Show AddSubExpr where
  show = \(FirstAndOpTermPairs term1 pairs) ->
    show term1 ++ concatMap ( \(op, term) -> show op ++ show term ) pairs

instance Show Equality where
  show = \(EqualityTerms term1 term2) -> show term1 ++ " = " ++ show term2

instance Show PureOpExpr where
  show = \case
    Equality equality -> show equality
    AddSubExpr add_sub_expr -> show add_sub_expr 

instance Show InputOpExpr where
  show = \(InputAndPureOpExpr input pure_op_expr) ->
    show input ++ show pure_op_expr

instance Show OperatorExpression where
  show = \case
    InputOpExpr input_op_expr -> show input_op_expr
    PureOpExpr pure_op_expr -> show pure_op_expr
