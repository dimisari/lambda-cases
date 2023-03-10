module Parsers.Values where

import Text.Parsec
  ( (<|>), try, char, many, many1, string, optionMaybe )
import Text.Parsec.String
  ( Parser )
import Text.Parsec.Combinator
  ( choice )

import Helpers
  ( (==>), seperated2, new_line_space_surrounded, space_or_newline
  , eof_or_new_lines )

import HaskellTypes.LowLevel
  ( Abstraction )
import HaskellTypes.Types
  ( ValueType )
import HaskellTypes.Values

import Parsers.LowLevel
  ( literal_p, value_name_p, abstraction_p )
import Parsers.Types
  ( value_type_p )

-- All:
-- ParenthesisValue, TupleValue, MathApplication, BaseValue
-- ApplicationDirection, FunctionApplicationChain
-- MultiplicationFactor, Multiplication, SubtractionFactor, Subtraction
-- EqualityFactor, Equality
-- OperatorExpression, AbstractionOpExpression, AbsOpOrOpExpression
-- LiteralOrValueName, SpecificCase, DefaultCase, Cases
-- NameTypeAndValue, NameTypeAndValueLists, NTAVOrNTAVLists, NamesTypesAndValues
-- Where, CasesOrWhere, AbstractionCasesOrWhere, ValueExpression

-- ParenthesisValue: parenthesis_value_p

parenthesis_value_p =
  char '(' *> (Parenthesis <$> abs_op_or_op_expr_p) <* char ')'
  :: Parser ParenthesisValue

-- TupleValue: tuple_value_p

tuple_value_p =
  char '(' >> abs_op_or_op_expr_p >>= \expr1 ->
  string ", " >> abs_op_or_op_expr_p >>= \expr2 ->
  many (string ", " >> abs_op_or_op_expr_p) >>= \exprs ->
  char ')' >> return (Values expr1 expr2 exprs)
  :: Parser TupleValue

-- MathApplication: math_application_p

math_application_p =  
  value_name_p >>= \vn ->
  char '(' >> abs_op_or_op_expr_p >>= \expr ->
  many (try $ string ", " >> abs_op_or_op_expr_p) >>= \exprs ->
  char ')' >> MathApp vn expr exprs==>return
  :: Parser MathApplication

-- BaseValue: base_value_p

base_value_p =
  ParenthesisValue <$> try parenthesis_value_p <|>
  TupleValue <$> tuple_value_p <|>
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
  base_val_app_dir_p >>= \bv_ad ->
  many (try base_val_app_dir_p) >>= \bv_ad_s ->
  base_value_p >>= \bv ->
  return $ ValuesAndDirections bv_ad bv_ad_s bv
  :: Parser FunctionApplicationChain

base_val_app_dir_p = 
  base_value_p >>= \bv -> application_direction_p >>= \ad -> return (bv, ad)
  :: Parser (BaseValue, ApplicationDirection)

-- MultiplicationFactor: multiplication_factor_p

multiplication_factor_p =
  OneArgAppMF <$> try function_app_chain_p <|> BaseValueMF <$> base_value_p
  :: Parser MultiplicationFactor

-- Multiplication: multiplication_p

multiplication_p =
  multiplication_factor_p >>= \mf1 ->
  string " * " >> multiplication_factor_p >>= \mf2 ->
  many (try $ string " * " >> multiplication_factor_p) >>= \mfs ->
  return $ Mul mf1 mf2 mfs
  :: Parser Multiplication

-- SubtractionFactor: subtraction_factor_p

subtraction_factor_p =
  MulSF <$> try multiplication_p <|> MFSF <$> multiplication_factor_p
  :: Parser SubtractionFactor

-- Subtraction: subtraction_p

subtraction_p =
  subtraction_factor_p >>= \sf1 -> string " - " >> subtraction_factor_p >>= \sf2 ->
  return $ Sub sf1 sf2
  :: Parser Subtraction

-- EqualityFactor: equality_factor_p

equality_factor_p =
  SubEF <$> try subtraction_p <|> SFEF <$> subtraction_factor_p
  :: Parser EqualityFactor

-- Equality: equality_p

equality_p =
  equality_factor_p >>= \ef1 -> string " = " >> equality_factor_p >>= \ef2 ->
  return $ Equ ef1 ef2
  :: Parser Equality

-- OperatorExpression: operator_expr_p

operator_expr_p =
  Equality <$> try equality_p <|> EquF <$> equality_factor_p
  :: Parser OperatorExpression

-- AbstractionOpExpression: abstraction_op_expr_p

abstraction_op_expr_p =
  abstraction_p >>= \abs -> operator_expr_p >>= \op_expr ->
  return $ AbstractionAndOpResult abs op_expr
  :: Parser AbstractionOpExpression

-- AbsOpOrOpExpression: abs_op_or_op_expr_p

abs_op_or_op_expr_p =
  AbstractionOpExpression <$> try abstraction_op_expr_p <|>
  OperatorExpression <$> operator_expr_p
  :: Parser AbsOpOrOpExpression

-- LiteralOrValueName: literal_or_value_name_p

literal_or_value_name_p =
  Lit <$> literal_p <|> ValName <$> value_name_p
  :: Parser LiteralOrValueName

-- SpecificCase: specific_case_p

specific_case_p =
  literal_or_value_name_p >>= \lit_or_val_name ->
  string " ->" >> space_or_newline >> value_expression_p >>= \v ->
  return $ SC lit_or_val_name v
  :: Parser SpecificCase

-- DefaultCase: default_case_p

default_case_p =
  string "... ->" >> space_or_newline >> value_expression_p >>= \v -> return $ DC v
  :: Parser DefaultCase

-- Cases: cases_p, one_and_default_cases_p, many_cases_p

cases_p =
  string "cases" >> (try one_and_default_cases_p <|> many_cases_p)
  :: Parser Cases

one_and_default_cases_p =
  new_line_space_surrounded >> specific_case_p >>= \sc ->
  new_line_space_surrounded >> default_case_p >>= \dc ->
  return $ OneAndDefault sc dc
  :: Parser Cases

many_cases_p =
  new_line_space_surrounded >> specific_case_p >>= \sc1 ->
  new_line_space_surrounded >> specific_case_p >>= \sc2 ->
  many (try $ new_line_space_surrounded >> specific_case_p) >>= \scs ->
  optionMaybe (try $ new_line_space_surrounded >> default_case_p) >>= \mdc ->
  return $ Many sc1 sc2 scs mdc
  :: Parser Cases

-- NameTypeAndValue: name_type_and_value_p
 
name_type_and_value_p =
  value_name_p >>= \vn -> string ": " >> value_type_p >>= \vt ->
  new_line_space_surrounded >> string "= " >> value_expression_p >>= \v ->
  return $ NTAV vn vt v
  :: Parser NameTypeAndValue

-- NameTypeAndValueLists: name_type_and_value_lists_p

name_type_and_value_lists_p = 
  seperated2 ", " value_name_p >>= \vns ->
  let
  value_types_p =
    seperated2 ", " value_type_p <|>
    (string "all " *> value_type_p >>= \vt -> return $ replicate (length vns) vt)
    :: Parser [ ValueType ]
  in
  string ": " >> value_types_p >>= \vts ->
  new_line_space_surrounded >> string "= " >> seperated2 ", " value_expression_p >>= \vs ->
  return $ NTAVLists vns vts vs
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
  names_types_and_values_p >>= \ns_ts_and_vs ->
  string "output" >> new_line_space_surrounded >>
  value_expression_p >>= \v ->
  return $ Where_ v ns_ts_and_vs
  :: Parser Where

-- CasesOrWhere: cases_or_where_p

cases_or_where_p = 
  Cases <$> try cases_p <|> Where <$> where_p
  :: Parser CasesOrWhere

-- AbstractionCasesOrWhere: abs_cases_or_where_p

abs_cases_or_where_p = 
  abstraction_p >>= \abs -> cases_or_where_p >>= \cases_or_where -> 
  return $ AbstractionAndCOWResult abs cases_or_where
  :: Parser AbstractionCasesOrWhere

-- ValueExpression: value_expression_p

value_expression_p =
  AbstractionCasesOrWhere <$> try abs_cases_or_where_p <|>
  CasesOrWhere <$> try cases_or_where_p <|>
  AbsOpOrOpExpression <$> abs_op_or_op_expr_p
  :: Parser ValueExpression
