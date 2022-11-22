module Parsers.Values where

import Text.Parsec
  ( (<|>), try, char, many1, string )
import Text.Parsec.String
  ( Parser )

import Helpers
  ( (==>), seperated2, new_line_space_surrounded, space_or_newline, eof_or_new_lines )

import HaskellTypes.LowLevel
  ( ApplicationDirection, Abstractions(..) )
import HaskellTypes.Types
  ( ValueType )
import HaskellTypes.Values

import Parsers.LowLevel
  ( value_name_p, literal_or_value_name_p, application_direction_p, abstraction_p
  , abstractions_p )
import Parsers.Types
  ( value_type_p )

{- 
  All:
  ParenthesisValue, BaseValue, OneArgApplications,
  MultiplicationFactor, xMultiplication, SubtractionFactor, Subtraction,
  NoAbstractionsValue1, ManyArgsArgValue, ManyArgsApplication,
  UseFields, SpecificCase, Cases,
  NameTypeAndValue, NameTypeAndValueLists,
  NTAVOrNTAVLists, NamesTypesAndValues, IntermediatesOutput,
  NoAbstractionsValue, Value
-}

-- ParenthesisValue
[ parenthesis_value_p, tuple_internals_p, parenthesis_internals_p ] =
  [ char '(' *> (try tuple_internals_p <|> parenthesis_internals_p) <* char ')'
  , fmap Tuple $ char ' ' *> seperated2 ", " value_p <* char ' '
  , Parenthesis <$> value_p ]
  :: [ Parser ParenthesisValue ]

-- BaseValue
base_value_p =
  ParenthesisValue <$> parenthesis_value_p <|>
  LiteralOrValueName <$> literal_or_value_name_p
  :: Parser BaseValue

-- OneArgApplications
one_arg_applications_p =
  many1 (try bv_ad_p) >>= \bv_ad_s -> 
  base_value_p >>= \bv ->
  return $ OAA bv_ad_s bv
  :: Parser OneArgApplications

bv_ad_p = 
  base_value_p >>= \bv ->
  application_direction_p >>= \ad ->
  return ( bv, ad )
  :: Parser ( BaseValue, ApplicationDirection )

-- MultiplicationFactor
multiplication_factor_p =
  OneArgAppMF <$> try one_arg_applications_p <|> BaseValueMF <$> base_value_p
  :: Parser MultiplicationFactor

-- Multiplication
multiplication_p =
  Mul <$> seperated2 " * " multiplication_factor_p
  :: Parser Multiplication

-- SubtractionFactor
subtraction_factor_p =
  MulSF <$> try multiplication_p <|>
  OneArgAppSF <$> try one_arg_applications_p <|>
  BaseValueSF <$> base_value_p 
  :: Parser SubtractionFactor

-- Subtraction
subtraction_p =
  subtraction_factor_p >>= \sf1 ->
  string " - " >> subtraction_factor_p >>= \sf2 ->
  return $ Sub sf1 sf2
  :: Parser Subtraction

-- EqualityFactor
equality_factor_p =
  SubEF <$> try subtraction_p <|>
  MulEF <$> try multiplication_p <|>
  OAAEF <$> try one_arg_applications_p <|>
  BaseValueEF <$> base_value_p 
  :: Parser EqualityFactor

-- Equality
equality_p =
  equality_factor_p >>= \ef1 ->
  string " = " >> equality_factor_p >>= \ef2 ->
  return $ Equ ef1 ef2
  :: Parser Equality

-- NoAbstractionsValue1
no_abstractions_value_1_p =
  Equality <$> try equality_p <|>
  Subtraction <$> try subtraction_p <|>
  Multiplication <$> try multiplication_p <|>
  OneArgApps <$> try one_arg_applications_p <|>
  BaseValue <$> base_value_p
  :: Parser NoAbstractionsValue1

-- ManyArgsArgValue
many_args_arg_value_p =
  try many_abstractions_arrow_maav_p <|> one_abstraction_arrow_maav_p
  :: Parser ManyArgsArgValue

one_abstraction_arrow_maav_p =
  abstractions_p >>= \as ->
  no_abstractions_value_1_p >>= \nae1 ->
  return $ MAAV as nae1
  :: Parser ManyArgsArgValue

many_abstractions_arrow_maav_p =
  seperated2 ", " abstraction_p >>= \as1 ->
  string " :->" >> space_or_newline >>
  one_abstraction_arrow_maav_p >>= \(MAAV (As as2) nav1) ->
  return $ MAAV (As $ as1 ++ as2) nav1
  :: Parser ManyArgsArgValue

-- ManyArgsApplication
many_args_application_p =
  seperated2 ", " many_args_arg_value_p >>= \maavs ->
  string " :==> " >> value_name_p >>= \vn ->
  return $ MAA maavs vn
  :: Parser ManyArgsApplication

-- UseFields
use_fields_p =
  string "use_fields ->" >> space_or_newline >> (UF <$> value_p)
  :: Parser UseFields

-- SpecificCase
specific_case_p =
  literal_or_value_name_p >>= \lovn ->
  string " ->" >> space_or_newline >> value_p >>= \v ->
  return $ SC lovn v
  :: Parser SpecificCase

-- Cases
cases_p =
  string "cases" >> new_line_space_surrounded >> specific_case_p >>= \c ->
  many1 (try $ new_line_space_surrounded >> specific_case_p) >>= \cs ->
  return $ Cs (c : cs)
  :: Parser Cases

-- NameTypeAndValue
name_type_and_value_p =
  value_name_p >>= \vn ->
  string ": " >> value_type_p >>= \vt ->
  new_line_space_surrounded >> string "= " >> value_p >>= \v ->
  eof_or_new_lines >> NTAV vn vt v==>return
  :: Parser NameTypeAndValue

-- NameTypeAndValueLists
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
  eof_or_new_lines >> NTAVLists vns vts vs==>return
  :: Parser NameTypeAndValueLists
  
-- NTAVOrNTAVLists
ntav_or_ntav_lists_p = 
  NameTypeAndValueLists <$> try name_type_and_value_lists_p <|>
  NameTypeAndValue <$> name_type_and_value_p
  :: Parser NTAVOrNTAVLists

-- NamesTypesAndValues
names_types_and_values_p =
  NTAVs <$> try ntav_or_ntav_lists_p==>many1
  :: Parser NamesTypesAndValues

-- IntermediatesOutput
intermediates_output_p = 
  string "intermediates" >> new_line_space_surrounded >>
  names_types_and_values_p >>= \ns_ts_and_vs ->
  string "output" >> new_line_space_surrounded >> value_p >>= \v ->
  return $ IntermediatesOutput_ ns_ts_and_vs v
  :: Parser IntermediatesOutput

-- NoAbstractionsValue
no_abstraction_expression_p =
  ManyArgsApplication <$> try many_args_application_p <|>
  Cases <$> try cases_p <|>
  IntermediatesOutput <$> try intermediates_output_p <|>
  UseFields <$> try use_fields_p <|>
  NoAbstractionsValue1 <$> no_abstractions_value_1_p 
  :: Parser NoAbstractionsValue

-- Value
value_p =
  try many_abstractions_arrow_value_p <|> one_abstraction_arrow_value_p
  :: Parser Value

one_abstraction_arrow_value_p =
  abstractions_p >>= \as ->
  no_abstraction_expression_p >>= \nae ->
  return $ Value as nae
  :: Parser Value

many_abstractions_arrow_value_p =
  seperated2 ", " abstraction_p >>= \as1 ->
  string " :->" >> space_or_newline >> one_abstraction_arrow_value_p >>=
  \(Value (As as2) nav) -> return $ Value (As $ as1 ++ as2) nav
  :: Parser Value
