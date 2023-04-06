module Parsers.Values where

import Text.Parsec
import Text.Parsec.String (Parser)

import Helpers 

import ParsingTypes.Types (ValueType)
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

-- ValueNamesTypesAndExpressions: value_names_types_and_expressions_p

value_types_p = ( \value_names_length ->
  sepBy1 value_type_p (string ", ") <|>
  (string "all " *> value_type_p >>= \value_type ->
  return $ replicate value_names_length value_type)
  ) :: Int -> Parser [ ValueType ]

value_names_types_and_expressions_p = 
  sepBy1 value_name_p (string ", ") >>= \value_names ->
  string ": " >> value_types_p (length value_names) >>= \value_types ->
  new_line_space_surrounded >> string "= " >>
  sepBy1 value_expression_p (string ", ") >>= \value_expressions ->
  case length value_types /= length value_names of 
    True -> 
      unexpected ": names and types don't match in numbers"
    False -> case length value_expressions /= length value_names of 
      True -> 
        unexpected ": names and expressions don't match in numbers"
      False ->
        return $ NamesTypesAndExpressions value_names value_types value_expressions
  :: Parser ValueNamesTypesAndExpressions

-- ValueOrValuesList: value_or_values_list_p

values_p =
  Values <$> try (value_names_types_and_expressions_p <* eof_or_new_lines)==>many1
  :: Parser Values

-- Where: where_p

where_p = 
  string "let" >> new_line_space_surrounded >>
  values_p >>= \names_types_and_values ->
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
