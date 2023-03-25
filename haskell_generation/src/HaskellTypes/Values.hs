module HaskellTypes.Values where

import Data.List ( intercalate )

import Helpers ( (==>), (.>) )

import HaskellTypes.LowLevel ( Literal, ValueName, Input )
import HaskellTypes.Types ( ValueType )

-- All: Types, Show instances

-- Types:
-- Parenthesis, Tuple, MathApplication, BaseValue
-- ApplicationDirection, FunctionApplicationChain
-- MultiplicationFactor, Multiplication, SubtractionFactor, Subtraction
-- EqualityFactor, Equality
-- OperatorExpression, InputOpExpression, InputOpExprOrOpExpr
-- LiteralOrValueName, SpecificCase, DefaultCase, Cases
-- NameTypeAndValue, NameTypeAndValueLists, NTAVOrNTAVLists, NamesTypesAndValues
-- Where, CasesOrWhere, InputCasesOrWhere, ValueExpression

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

data OperatorExpression =
  Equality Equality | EqualityFactor EqualityFactor

data InputOpExpression =
  InputAndOpResult Input OperatorExpression 

data InputOpExprOrOpExpr =
  InputOpExpression InputOpExpression |
  OperatorExpression OperatorExpression

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
-- Parenthesis, Tuple, MathApplication, BaseValue
-- ApplicationDirection, FunctionApplicationChain
-- MultiplicationFactor, Multiplication, SubtractionFactor, Subtraction
-- EqualityFactor, Equality
-- OperatorExpression, InputOpExpression, InputOpExprOrOpExpr
-- LiteralOrValueName, SpecificCase, DefaultCase, Cases
-- NameTypeAndValue, NameTypeAndValueLists, NTAVOrNTAVLists, NamesTypesAndValues
-- Where, CasesOrWhere, InputCasesOrWhere, ValueExpression

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

instance Show OperatorExpression where
  show = \case
    Equality equality -> show equality
    EqualityFactor factor -> show factor 

instance Show InputOpExpression where
  show = \(InputAndOpResult abs op_expr) -> show abs ++ show op_expr

instance Show InputOpExprOrOpExpr where
  show = \case
    InputOpExpression abs_op_expr -> show abs_op_expr
    OperatorExpression op_expr -> show op_expr

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
