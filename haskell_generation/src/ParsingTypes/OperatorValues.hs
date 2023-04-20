module ParsingTypes.OperatorValues where

import Data.List (intercalate)
import Helpers ((==>))
import ParsingTypes.LowLevel (Literal, ValueName, Input)

-- All: Types, Show instances

-- Types:
-- Parenthesis, Tuple, MathApplication, BaseValue
-- ApplicationDirection, FuncAppChain
-- MultiplicationFactor, Multiplication, SubtractionTerm, Subtraction
-- EqualityTerm, Equality
-- PureOpExpr, InputOpExpr, OperatorExpression

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
  ValuesAndDirections
    (BaseValue, ApplicationDirection)
    [ (BaseValue, ApplicationDirection) ]
    BaseValue

data MultiplicationFactor =
  FuncAppChain FuncAppChain | BaseValue BaseValue

data Multiplication =
  MultiplicationFactors
    MultiplicationFactor MultiplicationFactor [ MultiplicationFactor ]

data SubtractionTerm =
  Multiplication Multiplication | MultiplicationFactor MultiplicationFactor

data Subtraction =
  SubtractionTerms SubtractionTerm SubtractionTerm 

data AddSubTerm =
  Mult Multiplication | MultFactor MultiplicationFactor

data PlusOrMinus = 
  Plus | Minus

data AddSubExpr =
  FirstAndOpTermPairs AddSubTerm [ (PlusOrMinus, AddSubTerm) ]

data EqualityTerm =
  Subtraction Subtraction | SubtractionTerm SubtractionTerm

data Equality =
  EqualityTerms AddSubExpr AddSubExpr

data PureOpExpr =
  Equality Equality | AddSubExpr AddSubExpr

data InputOpExpr =
  InputAndPureOpExpr Input PureOpExpr 

data OperatorExpression =
  InputOpExpr InputOpExpr | PureOpExpr PureOpExpr

-- Show instances:
-- Parenthesis, Tuple, MathApplication, BaseValue
-- ApplicationDirection, FuncAppChain
-- MultiplicationFactor, Multiplication, SubtractionTerm, Subtraction
-- EqualityTerm, Equality
-- PureOpExpr, InputOpExpr, OperatorExpression

instance Show Parenthesis where
  show = \(InnerExpression expr) -> "(" ++ show expr ++ ")"

instance Show Tuple where
  show = \(TupleExpressions expr1 expr2 exprs) ->
    "(" ++ map show (expr1 : expr2 : exprs)==>intercalate ", " ++ ")"

instance Show MathApplication where
  show = \(NameAndInputExprs value_name expr1 exprs) ->
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

instance Show FuncAppChain where
  show = \(ValuesAndDirections base_val_app_dir base_val_app_dir_s base_val) ->
    concatMap
      ( \(base_val, app_dir) -> show base_val ++ show app_dir )
      (base_val_app_dir : base_val_app_dir_s)
    ++ show base_val

instance Show MultiplicationFactor where
  show = \case
    FuncAppChain func_app_chain -> show func_app_chain
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

instance Show AddSubTerm where
  show = \case
    Mult mult -> show mult
    MultFactor mult_fac -> show mult_fac

instance Show PlusOrMinus where
  show = \case
    Plus -> " + "
    Minus -> " - "

instance Show AddSubExpr where
  show = \(FirstAndOpTermPairs term1 pairs) ->
    show term1 ++ concatMap ( \(op, term) -> show op ++ show term ) pairs

instance Show EqualityTerm where
  show = \case
    Subtraction subtraction -> show subtraction
    SubtractionTerm subtraction_term -> show subtraction_term

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
