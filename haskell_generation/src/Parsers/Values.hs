module Parsers.Values where

import Text.Parsec ((<|>), try, char, many, many1, string, optionMaybe, unexpected)
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (choice)

import Helpers

import ParsingTypes.LowLevelValues (Abstraction)
import ParsingTypes.Types (ValueType)
import ParsingTypes.Values

import Parsers.LowLevelValues (literal_p, value_name_p, input_p)
import Parsers.Types (value_type_p)

-- All:
-- Parenthesis, Tuple, MathApplication, BaseValue
-- ApplicationDirection, FunctionApplicationChain
-- MultiplicationFactor, Multiplication, SubtractionFactor, Subtraction
-- EqualityFactor, Equality
-- OperatorExpression, InputOpExpression, InputOpExprOrOpExpr
-- LiteralOrValueName, SpecificCase, DefaultCase, Cases
-- NameTypeAndValue, NameTypeAndValueLists, NTAVOrNTAVLists, NamesTypesAndValues
-- Where, CasesOrWhere, InputCasesOrWhere, ValueExpression

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

-- OperatorExpression: operator_expr_p

operator_expr_p =
  Equality <$> try equality_p <|> EqualityFactor <$> equality_factor_p
  :: Parser OperatorExpression

-- InputOpExpression: input_op_expr_p

input_op_expr_p =
  input_p >>= \input -> operator_expr_p >>= \op_expr ->
  return $ InputAndOpResult input op_expr
  :: Parser InputOpExpression

-- InputOpExprOrOpExpr: abs_op_or_op_expr_p

abs_op_or_op_expr_p =
  InputOpExpression <$> try input_op_expr_p <|>
  OperatorExpression <$> operator_expr_p
  :: Parser InputOpExprOrOpExpr

-- LiteralOrValueName: literal_or_value_name_p

literal_or_value_name_p =
  Lit <$> literal_p <|> ValName <$> value_name_p
  :: Parser LiteralOrValueName

-- SpecificCase: specific_case_p

specific_case_p =
  literal_or_value_name_p >>= \lit_or_val_name ->
  string " ->" >> space_or_newline >> value_expression_p >>= \value_expression ->
  return $ SpecificCase lit_or_val_name value_expression
  :: Parser SpecificCase

-- DefaultCase: default_case_p

default_case_p =
  string "... ->" >> space_or_newline >> value_expression_p >>= \value_expr ->
  return $ DefaultCase value_expr
  :: Parser DefaultCase

-- Cases: cases_p, one_and_default_cases_p, many_cases_p

cases_p =
  string "cases" >> (try one_and_default_cases_p <|> many_cases_p)
  :: Parser Cases

one_and_default_cases_p =
  new_line_space_surrounded >> specific_case_p >>= \specific_case ->
  new_line_space_surrounded >> default_case_p >>= \default_case ->
  return $ OneAndDefault specific_case default_case
  :: Parser Cases

many_cases_p =
  new_line_space_surrounded >> specific_case_p >>= \specific_case1 ->
  new_line_space_surrounded >> specific_case_p >>= \specific_case2 ->
  many (try $ new_line_space_surrounded >> specific_case_p) >>= \specific_cases ->
  optionMaybe (try $ new_line_space_surrounded >> default_case_p) >>=
    \maybe_default_case ->
  return $ Many specific_case1 specific_case2 specific_cases maybe_default_case
  :: Parser Cases

-- NameTypeAndValue: name_type_and_value_p
 
name_type_and_value_p =
  value_name_p >>= \value_name -> string ": " >> value_type_p >>= \value_type ->
  new_line_space_surrounded >> string "= " >> value_expression_p >>= \value_expr ->
  return $ NTAV value_name value_type value_expr
  :: Parser NameTypeAndValue

-- NameTypeAndValueLists: name_type_and_value_lists_p

name_type_and_value_lists_p = 
  seperated2 ", " value_name_p >>= \value_names ->
  let
  value_types_p =
    seperated2 ", " value_type_p <|>
    (string "all " *> value_type_p >>= \value_type ->
    return $ replicate (length value_names) value_type)
    :: Parser [ ValueType ]
  in
  string ": " >> value_types_p >>= \value_types ->
  new_line_space_surrounded >> string "= " >>
  seperated2 ", " value_expression_p >>= \value_expressions ->
  case
  length value_types /= length value_names ||
  length value_expressions /= length value_names
  of 
  True -> 
    unexpected "value names, types and expressions don't match"
  False ->
    return $ NTAVLists value_names value_types value_expressions
  :: Parser NameTypeAndValueLists

-- NTAVOrNTAVLists: ntav_or_ntav_lists_p

ntav_or_ntav_lists_p = 
  NameTypeAndValueLists <$> try name_type_and_value_lists_p <|>
  NameTypeAndValue <$> name_type_and_value_p
  :: Parser NTAVOrNTAVLists

-- NamesTypesAndValues: names_types_and_values_p

names_types_and_values_p =
  NTAVs <$> try (ntav_or_ntav_lists_p <* eof_or_new_lines)==>many1
  :: Parser NamesTypesAndValues

-- Where: where_p

where_p = 
  string "let" >> new_line_space_surrounded >>
  names_types_and_values_p >>= \names_types_and_values ->
  string "output" >> new_line_space_surrounded >>
  value_expression_p >>= \value_expression ->
  return $ ValueWhereNTAVs value_expression names_types_and_values
  :: Parser Where

-- CasesOrWhere: cases_or_where_p

cases_or_where_p = 
  Cases <$> try cases_p <|> Where <$> where_p
  :: Parser CasesOrWhere

-- InputCasesOrWhere: input_cases_or_where_p

input_cases_or_where_p = 
  input_p >>= \input -> cases_or_where_p >>= \cases_or_where -> 
  return $ InputAndCOWResult input cases_or_where
  :: Parser InputCasesOrWhere

-- ValueExpression: value_expression_p

value_expression_p =
  InputCasesOrWhere <$> try input_cases_or_where_p <|>
  CasesOrWhere <$> try cases_or_where_p <|>
  InputOpExprOrOpExpr <$> abs_op_or_op_expr_p
  :: Parser ValueExpression
