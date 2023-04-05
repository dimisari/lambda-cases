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
-- MultiplicationFactor, Multiplication, SubtractionTerm, Subtraction
-- EqualityTerm, Equality
-- PureOperatorExpression, InputOperatorExpression, OperatorExpression

-- Parenthesis: parenthesis_p

parenthesis_p =
  char '(' *> (InnerExpression <$> operator_expression_p) <* char ')'
  :: Parser Parenthesis

-- Tuple: tuple_p

tuple_p =
  char '(' >> operator_expression_p >>= \op_expr1 ->
  string ", " >> operator_expression_p >>= \op_expr2 ->
  many (string ", " >> operator_expression_p) >>= \op_exprs ->
  char ')' >> return (TupleExpressions op_expr1 op_expr2 op_exprs)
  :: Parser Tuple

-- MathApplication: math_application_p

math_application_p =  
  value_name_p >>= \value_name ->
  char '(' >> operator_expression_p >>= \op_expr ->
  many (string ", " >> operator_expression_p) >>= \op_exprs ->
  char ')' >> return (NameAndInputExpressions value_name op_expr op_exprs)
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

-- FunctionApplicationChain:
-- function_application_chain_p, base_value_application_direction_p

function_application_chain_p =
  base_value_application_direction_p >>= \base_val_app_dir ->
  many (try base_value_application_direction_p) >>= \base_val_app_dirs ->
  base_value_p >>= \base_value ->
  return $ ValuesAndDirections base_val_app_dir base_val_app_dirs base_value
  :: Parser FunctionApplicationChain

base_value_application_direction_p = 
  base_value_p >>= \base_value ->
  application_direction_p >>= \application_direction ->
  return (base_value, application_direction)
  :: Parser (BaseValue, ApplicationDirection)

-- MultiplicationFactor: multiplication_factor_p

multiplication_factor_p =
  FunctionApplicationChain <$> try function_application_chain_p <|>
  BaseValue <$> base_value_p
  :: Parser MultiplicationFactor

-- Multiplication: multiplication_p

multiplication_p =
  multiplication_factor_p >>= \mul_factor1 ->
  string " * " >> multiplication_factor_p >>= \mul_factor2 ->
  many (try $ string " * " >> multiplication_factor_p) >>= \mul_factors ->
  return $ MultiplicationFactors mul_factor1 mul_factor2 mul_factors
  :: Parser Multiplication

-- SubtractionTerm: subtraction_term_p

subtraction_term_p =
  Multiplication <$> try multiplication_p <|>
  MultiplicationFactor <$> multiplication_factor_p
  :: Parser SubtractionTerm

-- Subtraction: subtraction_p

subtraction_p =
  subtraction_term_p >>= \subtraction_term1 ->
  string " - " >> subtraction_term_p >>= \subtraction_term2 ->
  return $ SubtractionTerms subtraction_term1 subtraction_term2
  :: Parser Subtraction

-- EqualityTerm: equality_term_p

equality_term_p =
  Subtraction <$> try subtraction_p <|>
  SubtractionTerm <$> subtraction_term_p
  :: Parser EqualityTerm

-- Equality: equality_p

equality_p =
  equality_term_p >>= \equality_term1 ->
  string " = " >> equality_term_p >>= \equality_term2 ->
  return $ EqualityTerms equality_term1 equality_term2
  :: Parser Equality

-- PureOperatorExpression: pure_operator_expression_p

pure_operator_expression_p =
  Equality <$> try equality_p <|> EqualityTerm <$> equality_term_p
  :: Parser PureOperatorExpression

-- InputOperatorExpression: input_operator_expression_p

input_operator_expression_p =
  input_p >>= \input -> pure_operator_expression_p >>= \op_expr ->
  return $ InputAndPureOperatorExpression input op_expr
  :: Parser InputOperatorExpression

-- OperatorExpression: operator_expression_p

operator_expression_p =
  InputOperatorExpression <$> try input_operator_expression_p <|>
  PureOperatorExpression <$> pure_operator_expression_p
  :: Parser OperatorExpression

