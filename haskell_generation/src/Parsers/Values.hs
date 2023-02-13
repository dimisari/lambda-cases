module Parsers.Values where

import Text.Parsec
  ( (<|>), try, char, many, many1, string )
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
  ( value_name_p, literal_or_value_name_p, abstraction_p )
import Parsers.Types
  ( value_type_p )

-- All:
-- ParenthesisValue, base_value_p, OneArgApplications,
-- multiplication_factor_p, multiplication_p, subtraction_factor_p, subtraction_p,
-- equality_factor_p, equality_p
-- operator_value_p, LambdaOperatorValue, many_args_application_p,
-- use_fields_p, specific_case_p, cases_p,
-- name_type_and_value_p, name_type_and_value_lists_p,
-- ntav_or_ntav_lists_p, names_types_and_values_p, where_p,
-- output_value_p, LambdaOutputValue

-- ParenthesisValue:
-- parenthesis_value_p, tuple_p
parenthesis_value_p =
  try tuple_p <|> char '(' *> (Parenthesis <$> value_p) <* char ')'
  :: Parser ParenthesisValue

tuple_p =
  char '(' >> lambda_operator_value_p >>= \lov1 ->
  string ", " >> lambda_operator_value_p >>= \lov2 ->
  many (try $ string ", " >> lambda_operator_value_p) >>= \lovs ->
  char ')' >> return (Tuple lov1 lov2 lovs)
  :: Parser ParenthesisValue
-- ParenthesisValue end

math_application_p =  
  value_name_p >>= \vn ->
  char '(' >> lambda_operator_value_p >>= \lov ->
  many (try $ string ", " >> lambda_operator_value_p) >>= \lovs ->
  char ')' >> MathApp vn lov lovs==>return
  :: Parser MathApplication

base_value_p =
  ParenthesisValue <$> try parenthesis_value_p <|>
  MathApplication <$> try math_application_p <|>
  LiteralOrValueName <$> literal_or_value_name_p
  :: Parser BaseValue

-- OneArgApplications: one_arg_applications_p, bv_ad_p
one_arg_applications_p =
  many1 (try bv_ad_p) >>= \(bv_ad : bv_ad_s) -> base_value_p >>= \bv ->
  return $ OAA bv_ad bv_ad_s bv
  :: Parser OneArgApplications

bv_ad_p = 
  base_value_p >>= \bv -> application_direction_p >>= \ad -> return (bv, ad)
  :: Parser (BaseValue, ApplicationDirection)

application_direction_p = 
  string "<==" *> return LeftApplication <|>
  string "==>" *> return RightApplication
  :: Parser ApplicationDirection
-- OneArgApplications end

multiplication_factor_p =
  OneArgAppMF <$> try one_arg_applications_p <|> BaseValueMF <$> base_value_p
  :: Parser MultiplicationFactor

multiplication_p =
  multiplication_factor_p >>= \mf1 ->
  string " * " >> multiplication_factor_p >>= \mf2 ->
  many (try $ string " * " >> multiplication_factor_p) >>= \mfs ->
  return $ Mul mf1 mf2 mfs
  :: Parser Multiplication

subtraction_factor_p =
  MulSF <$> try multiplication_p <|> MFSF <$> multiplication_factor_p
  :: Parser SubtractionFactor

subtraction_p =
  subtraction_factor_p >>= \sf1 -> string " - " >> subtraction_factor_p >>= \sf2 ->
  return $ Sub sf1 sf2
  :: Parser Subtraction

equality_factor_p =
  SubEF <$> try subtraction_p <|> SFEF <$> subtraction_factor_p
  :: Parser EqualityFactor

equality_p =
  equality_factor_p >>= \ef1 -> string " = " >> equality_factor_p >>= \ef2 ->
  return $ Equ ef1 ef2
  :: Parser Equality

operator_value_p =
  Equality <$> try equality_p <|> EquF <$> equality_factor_p
  :: Parser OperatorValue

-- LambdaOperatorValue:
-- lambda_operator_value_p, one_ab_arrow_lov_p, many_ab_arrow_lov_p
lambda_operator_value_p =
  try many_ab_arrow_lov_p <|> one_ab_arrow_lov_p
  :: Parser LambdaOperatorValue

many_ab_arrow_lov_p =
  seperated2 ", " abstraction_p >>= \as1 ->
  string " *->" >> space_or_newline >> one_ab_arrow_lov_p >>= \(LOV as2 nav1) ->
  return $ LOV (as1 ++ as2) nav1
  :: Parser LambdaOperatorValue

one_ab_arrow_lov_p =
  abstractions_p >>= \as -> operator_value_p >>= \nae1 -> return $ LOV as nae1
  :: Parser LambdaOperatorValue

abstractions_p =
  many (try $ abstraction_p <* string " -> ")
  :: Parser [ Abstraction ]
-- LambdaOperatorValue end

many_args_application_p =
  lambda_operator_value_p >>= \lov1 ->
  string ", " >>  lambda_operator_value_p >>= \lov2 ->
  many (try $ string ", " >> lambda_operator_value_p) >>= \lovs ->
  space_or_newline >> string "*==> " >> value_name_p >>= \vn ->
  return $ MAA lov1 lov2 lovs vn
  :: Parser ManyArgsApplication

-- use_fields_p =
--   string "use_fields ->" >> space_or_newline >> (UF <$> value_p)
--   :: Parser UseFields

specific_case_p =
  literal_or_value_name_p >>= \lovn ->
  string " ->" >> space_or_newline >> value_p >>= \v ->
  return $ SC lovn v
  :: Parser SpecificCase

cases_p =
  string "cases" >>
  new_line_space_surrounded >> specific_case_p >>= \c1 ->
  new_line_space_surrounded >> specific_case_p >>= \c2 ->
  many (try $ new_line_space_surrounded >> specific_case_p) >>= \cs ->
  return $ Cs c1 c2 cs
  :: Parser Cases

name_type_and_value_p =
  value_name_p >>= \vn -> string ": " >> value_type_p >>= \vt ->
  new_line_space_surrounded >> string "= " >> value_p >>= \v ->
  return $ NTAV vn vt v
  :: Parser NameTypeAndValue

name_type_and_value_lists_p = 
  seperated2 ", " value_name_p >>= \vns ->
  let
  value_types_p =
    seperated2 ", " value_type_p <|>
    (string "all " *> value_type_p >>= \vt -> return $ replicate (length vns) vt)
    :: Parser [ ValueType ]
  in
  string ": " >> value_types_p >>= \vts ->
  new_line_space_surrounded >> string "= " >> seperated2 ", " value_p >>= \vs ->
  return $ NTAVLists vns vts vs
  :: Parser NameTypeAndValueLists

ntav_or_ntav_lists_p = 
  NameTypeAndValueLists <$> try name_type_and_value_lists_p <|>
  NameTypeAndValue <$> name_type_and_value_p
  :: Parser NTAVOrNTAVLists

names_types_and_values_p =
  NTAVs <$> try (ntav_or_ntav_lists_p <* eof_or_new_lines) ==>many1
  :: Parser NamesTypesAndValues

where_p = 
  string "let" >> new_line_space_surrounded >>
  names_types_and_values_p >>= \ns_ts_and_vs ->
  string "output" >> new_line_space_surrounded >>
  value_p >>= \v ->
  return $ Where_ v ns_ts_and_vs
  :: Parser Where

output_value_p = choice $
  [ ManyArgsApplication <$> try many_args_application_p
  , Cases <$> try cases_p
  , Where <$> try where_p
  --, UseFields <$> try use_fields_p
  , OperatorValue <$> operator_value_p
  ] :: Parser OutputValue

-- LambdaOutputValue:
-- value_p, one_abstraction_arrow_value_p, many_abstractions_arrow_value_p
value_p =
  try many_abstractions_arrow_value_p <|> one_abstraction_arrow_value_p
  :: Parser LambdaOutputValue

one_abstraction_arrow_value_p =
  abstractions_p >>= \as -> output_value_p >>= \nae ->
  return $ LV as nae
  :: Parser LambdaOutputValue

many_abstractions_arrow_value_p =
  seperated2 ", " abstraction_p >>= \as1 ->
  string " *->" >> space_or_newline >> one_abstraction_arrow_value_p >>=
    \(LV as2 nav) -> return $ LV (as1 ++ as2) nav
  :: Parser LambdaOutputValue
