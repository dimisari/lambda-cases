module Parsers.Values where

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
import Parsers.OperatorValues (operator_expression_p)

-- All:
-- CaseLiteralOrValueName, SpecificCase, DefaultCase, Cases
-- ValueNameTypeAndExpression, ValueNamesTypesAndExpressions,
-- ValueOrValues, ValueOrValuesList
-- Where, CasesOrWhere, InputCasesOrWhere, ValueExpression

-- CaseLiteralOrValueName: case_literal_or_value_name_p

case_literal_or_value_name_p =
  CaseLiteral <$> literal_p <|> CaseValueName <$> value_name_p
  :: Parser CaseLiteralOrValueName

-- SpecificCase: specific_case_p

specific_case_p =
  case_literal_or_value_name_p >>= \case_literal_or_value_name_p ->
  string " ->" >> space_or_newline >> value_expression_p >>= \value_expression ->
  return $ SpecificCase case_literal_or_value_name_p value_expression
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

-- ValueNameTypeAndExpression: value_name_type_and_expression_p
 
value_name_type_and_expression_p =
  value_name_p >>= \value_name -> string ": " >> value_type_p >>= \value_type ->
  new_line_space_surrounded >> string "= " >> value_expression_p >>= \value_expr ->
  return $ NameTypeAndExpression value_name value_type value_expr
  :: Parser ValueNameTypeAndExpression

-- ValueNamesTypesAndExpressions: value_names_types_and_expressions_p

value_names_types_and_expressions_p = 
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
    return $ NamesTypesAndExpressions value_names value_types value_expressions
  :: Parser ValueNamesTypesAndExpressions

-- ValueOrValues: value_or_values_p

value_or_values_p = 
  ValueNamesTypesAndExpressions <$> try value_names_types_and_expressions_p <|>
  ValueNameTypeAndExpression <$> value_name_type_and_expression_p
  :: Parser ValueOrValues

-- ValueOrValuesList: value_or_values_list_p

value_or_values_list_p =
  ValueOrValuesList <$> try (value_or_values_p <* eof_or_new_lines)==>many1
  :: Parser ValueOrValuesList

-- Where: where_p

where_p = 
  string "let" >> new_line_space_surrounded >>
  value_or_values_list_p >>= \names_types_and_values ->
  string "output" >> new_line_space_surrounded >>
  value_expression_p >>= \value_expression ->
  return $ ValueExpressionWhereValues value_expression names_types_and_values
  :: Parser Where

-- CasesOrWhere: cases_or_where_p

cases_or_where_p = 
  Cases <$> try cases_p <|> Where <$> where_p
  :: Parser CasesOrWhere

-- InputCasesOrWhere: input_cases_or_where_p

input_cases_or_where_p = 
  input_p >>= \input -> cases_or_where_p >>= \cases_or_where -> 
  return $ InputAndCasesOrWhere input cases_or_where
  :: Parser InputCasesOrWhere

-- ValueExpression: value_expression_p

value_expression_p =
  InputCasesOrWhere <$> try input_cases_or_where_p <|>
  CasesOrWhere <$> try cases_or_where_p <|>
  OperatorExpression <$> operator_expression_p
  :: Parser ValueExpression
