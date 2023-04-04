module ParsingTypes.OperatorValues where

import Data.List (intercalate)

import Helpers ((==>), (.>))

import ParsingTypes.LowLevel (Literal, ValueName, Input)
import ParsingTypes.Types (ValueType)

-- All: Types, Show instances

-- Types:
-- Parenthesis, Tuple, MathApplication, BaseValue
-- ApplicationDirection, FunctionApplicationChain
-- MultiplicationFactor, Multiplication, SubtractionFactor, Subtraction
-- EqualityFactor, Equality
-- PureOperatorExpression, InputOperatorExpression, InputOpExprOrOpExpr

newtype Parenthesis =
  InnerExpression InputOpExprOrOpExpr 

data Tuple =
  Values InputOpExprOrOpExpr InputOpExprOrOpExpr [ InputOpExprOrOpExpr ]

data MathApplication =
  MathApp ValueName InputOpExprOrOpExpr [ InputOpExprOrOpExpr ]

data BaseValue =
  Parenthesis Parenthesis | Tuple Tuple | Literal Literal | ValueName ValueName |
  MathApplication MathApplication

data ApplicationDirection =
  LeftApplication | RightApplication

data FunctionApplicationChain =
  ValuesAndDirections
    (BaseValue, ApplicationDirection)
    [ (BaseValue, ApplicationDirection) ]
    BaseValue

data MultiplicationFactor =
  FuncAppChain FunctionApplicationChain | BaseValue BaseValue

data Multiplication =
  MulFactors MultiplicationFactor MultiplicationFactor [ MultiplicationFactor ]

data SubtractionFactor =
  Multiplication Multiplication | MultiplicationFactor MultiplicationFactor

data Subtraction =
  SubFactors SubtractionFactor SubtractionFactor 

data EqualityFactor =
  Subtraction Subtraction | SubtractionFactor SubtractionFactor

data Equality =
  EqualityFactors EqualityFactor EqualityFactor

data PureOperatorExpression =
  Equality Equality | EqualityFactor EqualityFactor

data InputOperatorExpression =
  InputAndPureOpExpr Input PureOperatorExpression 

data InputOpExprOrOpExpr =
  InputOperatorExpression InputOperatorExpression |
  PureOperatorExpression PureOperatorExpression

-- Show instances:
-- Parenthesis, Tuple, MathApplication, BaseValue
-- ApplicationDirection, FunctionApplicationChain
-- MultiplicationFactor, Multiplication, SubtractionFactor, Subtraction
-- EqualityFactor, Equality
-- PureOperatorExpression, InputOperatorExpression, InputOpExprOrOpExpr

instance Show Parenthesis where
  show = \(InnerExpression expr) -> "(" ++ show expr ++ ")"

instance Show Tuple where
  show = \(Values expr1 expr2 exprs) ->
    "(" ++ map show (expr1 : expr2 : exprs)==>intercalate ", " ++ ")"

instance Show MathApplication where
  show = \(MathApp value_name expr1 exprs) ->
    show value_name ++ "(" ++ (expr1 : exprs)==>map show==>intercalate ", " ++ ")"

instance Show BaseValue where
  show = \case
    Parenthesis parenthesis_value -> show parenthesis_value
    Tuple tuple -> show tuple
    Literal literal -> show literal
    ValueName value_name -> show value_name
    MathApplication math_application -> show math_application

instance Show ApplicationDirection where
  show = \case
    LeftApplication -> "<=="
    RightApplication -> "==>"

instance Show FunctionApplicationChain where
  show = \(ValuesAndDirections base_val_app_dir base_val_app_dir_s base_val) ->
    concatMap
      (\(base_val, app_dir) -> show base_val ++ show app_dir)
      (base_val_app_dir : base_val_app_dir_s)
    ++ show base_val

instance Show MultiplicationFactor where
  show = \case
    FuncAppChain func_app_chain -> show func_app_chain
    BaseValue base_value -> show base_value

instance Show Multiplication where
  show = \(MulFactors factor1 factor2 factors) ->
    (factor1 : factor2 : factors)==>map show==>intercalate " * "

instance Show SubtractionFactor where
  show = \case
    Multiplication m -> show m
    MultiplicationFactor f -> show f

instance Show Subtraction where
  show = \(SubFactors factor1 factor2) -> show factor1 ++ " - " ++ show factor2

instance Show EqualityFactor where
  show = \case
    Subtraction s -> show s
    SubtractionFactor f -> show f

instance Show Equality where
  show = \(EqualityFactors factor1 factor2) ->
    show factor1 ++ " = " ++ show factor2

instance Show PureOperatorExpression where
  show = \case
    Equality equality -> show equality
    EqualityFactor factor -> show factor 

instance Show InputOperatorExpression where
  show = \(InputAndPureOpExpr abs op_expr) -> show abs ++ show op_expr

instance Show InputOpExprOrOpExpr where
  show = \case
    InputOperatorExpression abs_op_expr -> show abs_op_expr
    PureOperatorExpression op_expr -> show op_expr
