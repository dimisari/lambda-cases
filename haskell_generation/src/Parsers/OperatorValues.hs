module Parsers.OperatorValues where

import Text.Parsec ((<|>), try, char, many, many1, string, optionMaybe, unexpected)
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (choice)

import Helpers

import ParsingTypes.LowLevel (Abstraction)
import ParsingTypes.Types (ValueType)
import ParsingTypes.OperatorValues
import ParsingTypes.Values

import Parsers.LowLevel (literal_p, value_name_p, input_p)
import Parsers.Types (value_type_p)

-- All:
-- Parenthesis, Tuple, MathApplication, BaseValue
-- ApplicationDirection, FunctionApplicationChain
-- MultiplicationFactor, Multiplication, SubtractionFactor, Subtraction
-- EqualityFactor, Equality
-- PureOperatorExpression, InputOperatorExpression, InputOpExprOrOpExpr

-- Parenthesis: parenthesis_p

parenthesis_p =
  char '(' *> (InnerExpression <$> abs_op_or_op_expr_p) <* char ')'
  :: Parser Parenthesis

-- Tuple: tuple_p

tuple_p =
  char '(' >> abs_op_or_op_expr_p >>= \expr1 ->
  string ", " >> abs_op_or_op_expr_p >>= \expr2 ->
  many (string ", " >> abs_op_or_op_expr_p) >>= \exprs ->
  char ')' >> return (Values expr1 expr2 exprs)
  :: Parser Tuple

-- MathApplication: math_application_p

math_application_p =  
  value_name_p >>= \value_name ->
  char '(' >> abs_op_or_op_expr_p >>= \expr ->
  many (string ", " >> abs_op_or_op_expr_p) >>= \exprs ->
  char ')' >> MathApp value_name expr exprs==>return
  :: Parser MathApplication

-- BaseValue: base_value_p

base_value_p =
  Parenthesis <$> try parenthesis_p <|>
  Tuple <$> tuple_p <|>
  MathApplication <$> try math_application_p <|>
  Literal <$> literal_p <|>
  ValueName <$> value_name_p
  :: Parser BaseValue

-- ApplicationDirection: application_direction_p

application_direction_p = 
  string "<==" *> return LeftApplication <|>
  string "==>" *> return RightApplication
  :: Parser ApplicationDirection

-- FunctionApplicationChain: function_app_chain_p, base_val_app_dir_p

function_app_chain_p =
  base_val_app_dir_p >>= \base_val_app_dir ->
  many (try base_val_app_dir_p) >>= \base_val_app_dir_s ->
  base_value_p >>= \base_value ->
  return $ ValuesAndDirections base_val_app_dir base_val_app_dir_s base_value
  :: Parser FunctionApplicationChain

base_val_app_dir_p = 
  base_value_p >>= \base_value ->
  application_direction_p >>= \application_direction ->
  return (base_value, application_direction)
  :: Parser (BaseValue, ApplicationDirection)

-- MultiplicationFactor: multiplication_factor_p

multiplication_factor_p =
  FuncAppChain <$> try function_app_chain_p <|> BaseValue <$> base_value_p
  :: Parser MultiplicationFactor

-- Multiplication: multiplication_p

multiplication_p =
  multiplication_factor_p >>= \mul_factor1 ->
  string " * " >> multiplication_factor_p >>= \mul_factor2 ->
  many (try $ string " * " >> multiplication_factor_p) >>= \mul_factors ->
  return $ MulFactors mul_factor1 mul_factor2 mul_factors
  :: Parser Multiplication

-- SubtractionFactor: subtraction_factor_p

subtraction_factor_p =
  Multiplication <$> try multiplication_p <|>
  MultiplicationFactor <$> multiplication_factor_p
  :: Parser SubtractionFactor

-- Subtraction: subtraction_p

subtraction_p =
  subtraction_factor_p >>= \subtraction_factor1 ->
  string " - " >> subtraction_factor_p >>= \subtraction_factor2 ->
  return $ SubFactors subtraction_factor1 subtraction_factor2
  :: Parser Subtraction

-- EqualityFactor: equality_factor_p

equality_factor_p =
  Subtraction <$> try subtraction_p <|>
  SubtractionFactor <$> subtraction_factor_p
  :: Parser EqualityFactor

-- Equality: equality_p

equality_p =
  equality_factor_p >>= \equality_factor1 ->
  string " = " >> equality_factor_p >>= \equality_factor2 ->
  return $ EqualityFactors equality_factor1 equality_factor2
  :: Parser Equality

-- PureOperatorExpression: operator_expr_p

operator_expr_p =
  Equality <$> try equality_p <|> EqualityFactor <$> equality_factor_p
  :: Parser PureOperatorExpression

-- InputOperatorExpression: input_op_expr_p

input_op_expr_p =
  input_p >>= \input -> operator_expr_p >>= \op_expr ->
  return $ InputAndPureOpExpr input op_expr
  :: Parser InputOperatorExpression

-- InputOpExprOrOpExpr: abs_op_or_op_expr_p

abs_op_or_op_expr_p =
  InputOperatorExpression <$> try input_op_expr_p <|>
  PureOperatorExpression <$> operator_expr_p
  :: Parser InputOpExprOrOpExpr

