module ParsingTypes.OperatorValues where

import Data.List (intercalate)

import Helpers ((==>), (.>))

import ParsingTypes.LowLevel (Literal, ValueName, Input)
import ParsingTypes.Types (ValueType)

-- All: Types, Show instances

-- Types:
-- Parenthesis, Tuple, MathApplication, BaseValue
-- ApplicationDirection, FunctionApplicationChain
-- MultiplicationFactor, Multiplication, SubtractionTerm, Subtraction
-- EqualityTerm, Equality
-- PureOperatorExpression, InputOperatorExpression, OperatorExpression

newtype Parenthesis =
  InnerExpression OperatorExpression 

data Tuple =
  TupleExpressions OperatorExpression OperatorExpression [ OperatorExpression ]

data MathApplication =
  NameAndInputExpressions ValueName OperatorExpression [ OperatorExpression ]

data BaseValue =
  Literal Literal |
  ValueName ValueName |
  Parenthesis Parenthesis |
  Tuple Tuple |
  MathApplication MathApplication

data ApplicationDirection =
  LeftApplication | RightApplication

data FunctionApplicationChain =
  ValuesAndDirections
    (BaseValue, ApplicationDirection)
    [ (BaseValue, ApplicationDirection) ]
    BaseValue

data MultiplicationFactor =
  FunctionApplicationChain FunctionApplicationChain | BaseValue BaseValue

data Multiplication =
  MultiplicationFactors
    MultiplicationFactor MultiplicationFactor [ MultiplicationFactor ]

data SubtractionTerm =
  Multiplication Multiplication | MultiplicationFactor MultiplicationFactor

data Subtraction =
  SubtractionTerms SubtractionTerm SubtractionTerm 

data EqualityTerm =
  Subtraction Subtraction | SubtractionTerm SubtractionTerm

data Equality =
  EqualityTerms EqualityTerm EqualityTerm

data PureOperatorExpression =
  Equality Equality | EqualityTerm EqualityTerm

data InputOperatorExpression =
  InputAndPureOperatorExpression Input PureOperatorExpression 

data OperatorExpression =
  InputOperatorExpression InputOperatorExpression |
  PureOperatorExpression PureOperatorExpression

-- Show instances:
-- Parenthesis, Tuple, MathApplication, BaseValue
-- ApplicationDirection, FunctionApplicationChain
-- MultiplicationFactor, Multiplication, SubtractionTerm, Subtraction
-- EqualityTerm, Equality
-- PureOperatorExpression, InputOperatorExpression, OperatorExpression

instance Show Parenthesis where
  show = \(InnerExpression expr) -> "(" ++ show expr ++ ")"

instance Show Tuple where
  show = \(TupleExpressions expr1 expr2 exprs) ->
    "(" ++ map show (expr1 : expr2 : exprs)==>intercalate ", " ++ ")"

instance Show MathApplication where
  show = \(NameAndInputExpressions value_name expr1 exprs) ->
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
      ( \(base_val, app_dir) -> show base_val ++ show app_dir )
      (base_val_app_dir : base_val_app_dir_s)
    ++ show base_val

instance Show MultiplicationFactor where
  show = \case
    FunctionApplicationChain func_app_chain -> show func_app_chain
    BaseValue base_value -> show base_value

instance Show Multiplication where
  show = \(MultiplicationFactors f1 f2 fs) ->
    (f1 : f2 : fs)==>map show==>intercalate " * "

instance Show SubtractionTerm where
  show = \case
    Multiplication multiplication -> show multiplication
    MultiplicationFactor multiplication_factor -> show multiplication_factor

instance Show Subtraction where
  show = \(SubtractionTerms term1 term2) -> show term1 ++ " - " ++ show term2

instance Show EqualityTerm where
  show = \case
    Subtraction subtraction -> show subtraction
    SubtractionTerm subtraction_term -> show subtraction_term

instance Show Equality where
  show = \(EqualityTerms term1 term2) -> show term1 ++ " = " ++ show term2

instance Show PureOperatorExpression where
  show = \case
    Equality equality -> show equality
    EqualityTerm equality_term -> show equality_term 

instance Show InputOperatorExpression where
  show = \(InputAndPureOperatorExpression input pure_op_expr) ->
    show input ++ show pure_op_expr

instance Show OperatorExpression where
  show = \case
    InputOperatorExpression input_op_expr -> show input_op_expr
    PureOperatorExpression pure_op_expr -> show pure_op_expr
