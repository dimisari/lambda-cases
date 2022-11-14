module Parsers.Values where

import Prelude
  ( (<$>), (<*), (*>), (++), ($), (>>=), (>>), return, fmap, replicate, length )
import Text.Parsec ( (<|>), try, char, many1, string, eof, skipMany1 )
import Text.Parsec.String ( Parser )

import Helpers
  ( (-->), seperated2, comma_seperated2, spaces_tabs, new_line_space_surrounded )

import HaskellTypes.LowLevel ( ApplicationDirection, Abstractions(..) )
import Parsers.LowLevel
  ( value_name_p, literal_or_value_name_p, application_direction_p, abstraction_p
  , abstractions_p )

import HaskellTypes.Types ( ValueType )
import Parsers.Types ( value_type_p )

import HaskellTypes.Values
  ( ParenthesisValue(..), ParenLitOrName(..), OneArgApplications(..)
  , MultiplicationFactor(..), Multiplication(..), SubtractionFactor(..), Subtraction(..)
  , NoAbstractionsValue1(..), ManyArgsArgValue(..), ManyArgsApplication(..)
  , UseFields(..), SpecificCase(..), Cases(..)
  , NameTypeAndValue(..), NameTypeAndValueLists(..)
  , NTAVOrNTAVLists(..), NamesTypesAndValues(..)
  , IntermediatesOutput(..)
  , NoAbstractionsValue(..), Value(..)
  )

{- 
  All:
  ParenthesisValue, ParenLitOrName, OneArgApplications,
  MultiplicationFactor, Multiplication, SubtractionFactor, Subtraction,
  NoAbstractionsValue1, ManyArgsArgValue, ManyArgsApplication,
  UseFields, SpecificCase, Cases,
  NameTypeAndValue, NameTypeAndValueLists,
  NTAVOrNTAVLists, NamesTypesAndValues, IntermediatesOutput,
  NoAbstractionsValue, Value
-}

-- ParenthesisValue

[ parenthesis_value_p, tuple_internals_p, parenthesis_internals_p ] =
  [ char '(' *> (try tuple_internals_p <|> parenthesis_internals_p) <* char ')'
  , fmap Tuple $ char ' ' *> comma_seperated2 value_p <* char ' '
  , Parenthesis <$> value_p ]
  :: [ Parser ParenthesisValue ]

-- ParenLitOrName

paren_lit_or_name_p =
  ParenthesisValue <$> parenthesis_value_p <|>
  LiteralOrValueName <$> literal_or_value_name_p
  :: Parser ParenLitOrName

-- OneArgApplications

one_arg_applications_p =
  paren_lit_or_name_p >>= \plon ->
  many1 ad_plon_p >>= \ad_plon_s -> 
  return $ OAA plon ad_plon_s
  :: Parser OneArgApplications

ad_plon_p = 
  application_direction_p >>= \ad ->
  paren_lit_or_name_p >>= \plon ->
  return ( ad, plon )
  :: Parser ( ApplicationDirection, ParenLitOrName )

-- MultiplicationFactor

multiplication_factor_p =
  OneArgAppMF <$> try one_arg_applications_p <|> ParenLitOrNameMF <$> paren_lit_or_name_p
  :: Parser MultiplicationFactor

-- Multiplication

multiplication_p =
  Mul <$> seperated2 multiplication_factor_p " * "
  :: Parser Multiplication

-- SubtractionFactor

subtraction_factor_p =
  MulSF <$> try multiplication_p <|> OAASF <$> try one_arg_applications_p <|>
  ParenLitOrNameSF <$> paren_lit_or_name_p 
  :: Parser SubtractionFactor

-- Subtraction

subtraction_p =
  subtraction_factor_p >>= \sf1 ->
  string " - " >> subtraction_factor_p >>= \sf2 ->
  return $ Sub sf1 sf2
  :: Parser Subtraction

-- NoAbstractionsValue1

no_abstractions_value_1_p =
  Subtraction <$> try subtraction_p <|> Multiplication <$> try multiplication_p <|>
  OneArgApps <$> try one_arg_applications_p <|> PLON <$> paren_lit_or_name_p
  :: Parser NoAbstractionsValue1

-- ManyArgsArgValue

many_args_arg_value_p =
  try many_args_arg_value2_p <|> many_args_arg_value1_p
  :: Parser ManyArgsArgValue

many_args_arg_value1_p =
  abstractions_p >>= \as ->
  no_abstractions_value_1_p >>= \nae1 ->
  return $ MAAV as nae1
  :: Parser ManyArgsArgValue

many_args_arg_value2_p =
  comma_seperated2 abstraction_p >>= \as1 ->
  string " :> " >>
  many_args_arg_value1_p >>= \(MAAV (As as2) nav1) ->
  return $ MAAV (As $ as1 ++ as2) nav1
  :: Parser ManyArgsArgValue

-- ManyArgsApplication

many_args_application_p =
  comma_seperated2 many_args_arg_value_p >>= \maavs ->
  string " :-> " >> value_name_p >>= \vn ->
  return $ MAA maavs vn
  :: Parser ManyArgsApplication

-- UseFields

use_fields_p =
  fmap UF $ string "use_fields -> " *> value_p
  :: Parser UseFields

-- SpecificCase

specific_case_p =
  spaces_tabs >> literal_or_value_name_p >>= \lovn ->
  string " ->" >> (try new_line_space_surrounded <|> (char ' ')) >> value_p >>= \v ->
  return $ SC lovn v
  :: Parser SpecificCase

-- Cases

cases_p =
  fmap Cs $ string "cases\n" *> many1 (specific_case_p <* char '\n')
  :: Parser Cases

-- NameTypeAndValue

name_type_and_value_p =
  value_name_p >>= \vn ->
  string ": " >> value_type_p >>= \vt ->
  new_line_space_surrounded >> string "= " >> value_p >>= \v ->
  (eof <|> skipMany1 new_line_space_surrounded) >> NTAV vn vt v-->return
  :: Parser NameTypeAndValue

-- NameTypeAndValueLists

name_type_and_value_lists_p = 
  spaces_tabs >> comma_seperated2 value_name_p >>= \vns ->
  let
  value_types_p =
    comma_seperated2 value_type_p <|>
    (string "all " *> value_type_p >>= \vt -> return $ replicate (length vns) vt)
    :: Parser [ ValueType ]
  in
  string ": " >> value_types_p >>= \vts ->
  new_line_space_surrounded >> string "= " >> comma_seperated2 value_p >>= \vs ->
  (eof <|> skipMany1 new_line_space_surrounded) >> NTAVLists vns vts vs-->return
  :: Parser NameTypeAndValueLists
  
-- NTAVOrNTAVLists

ntav_or_ntav_lists_p = 
  NameTypeAndValueLists <$> try name_type_and_value_lists_p <|>
  NameTypeAndValue <$> name_type_and_value_p
  :: Parser NTAVOrNTAVLists

-- NamesTypesAndValues

names_types_and_values_p =
  NTAVs <$> try ntav_or_ntav_lists_p-->many1
  :: Parser NamesTypesAndValues

-- IntermediatesOutput

intermediates_output_p = 
  spaces_tabs >> string "intermediates" >> new_line_space_surrounded >>
  names_types_and_values_p >>= \ns_ts_and_vs ->
  spaces_tabs >> string "output" >> new_line_space_surrounded >>
  value_p >>= \v ->
  return $ IntermediatesOutput_ ns_ts_and_vs v
  :: Parser IntermediatesOutput

-- NoAbstractionsValue

no_abstraction_expression_p =
  ManyArgsApplication <$> try many_args_application_p <|> Cases <$> try cases_p <|>
  IntermediatesOutput <$> try intermediates_output_p <|>
  UseFields <$> try use_fields_p <|> NoAbstractionsValue1 <$> no_abstractions_value_1_p 
  :: Parser NoAbstractionsValue

-- Value

value_p = try value2_p <|> value1_p
  :: Parser Value

value1_p =
  abstractions_p >>= \as ->
  no_abstraction_expression_p >>= \nae ->
  return $ Value as nae
  :: Parser Value

value2_p =
  comma_seperated2 abstraction_p >>= \as1 ->
  string " :> " >> value1_p >>= \(Value (As as2) nav) ->
  return $ Value (As $ as1 ++ as2) nav
  :: Parser Value
