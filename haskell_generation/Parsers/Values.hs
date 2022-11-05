{-# LANGUAGE LambdaCase #-}

module Parsers.Values where

import Prelude ( (<$>), (<*), (*>), (++), ($), (>>=), (>>), return, fmap )
import Text.Parsec ( (<|>), try, char, many1, string, eof, skipMany1 )
import Text.Parsec.String ( Parser )

import Helpers
  ( (-->), seperated2, comma_seperated2, spaces_tabs, new_line_space_surrounded )

import HaskellTypes.LowLevel ( ApplicationDirection, Abstractions ( Abstractions ) )
import Parsers.LowLevel
  ( value_name_p, literal_or_value_name_p, application_direction_p, abstraction_p
  , abstractions_p )
import Parsers.Types ( value_type_p )

import HaskellTypes.Values
  ( ParenthesisValue(..), ParenLitOrName(..), OneArgFunctionApplications(..)
  , MultiplicationFactor(..), Multiplication(..), SubtractionFactor(..), Subtraction(..)
  , NoAbstractionsValue1(..), ManyArgsAppArgValue(..), ManyArgsApplication(..)
  , SpecificCase(..), Cases(..)
  , NameTypeAndValue(..), NameTypeAndValueLists(..)
  , NTAVOrNTAVLists(..), NamesTypesAndValues(..)
  , IntermediatesOutput(..)
  , NoAbstractionsValue(..), Value(..)
  )

{- 
  All:
  ParenthesisValue, ParenLitOrName, OneArgFunctionApplications,
  MultiplicationFactor, Multiplication, SubtractionFactor, Subtraction,
  NoAbstractionsValue1, ManyArgsAppArgValue, ManyArgsApplication,
  SpecificCase, Cases,
  NameTypeAndValue, NameTypeAndValueLists,
  NTAVOrNTAVLists, NamesTypesAndValues, IntermediatesOutput,
  NoAbstractionsValue, Value
-}

-- ParenthesisValue

[ parenthesis_value_p, tuple_internals_p, parenthesis_internals_p ] =
  [ char '(' *> (try tuple_internals_p <|> parenthesis_internals_p) <* char ')'
  , fmap Tuple $ char ' ' *> comma_seperated2 value_expression_p <* char ' '
  , Parenthesis <$> value_expression_p ]
  :: [ Parser ParenthesisValue ]

-- ParenLitOrName

paren_lit_or_name_p =
  ParenthesisValue <$> parenthesis_value_p <|>
  LiteralOrValueName <$> literal_or_value_name_p
  :: Parser ParenLitOrName

-- OneArgFunctionApplications

one_arg_function_applications_p =
  paren_lit_or_name_p >>= \plon ->
  many1 ad_plon_p >>= \ad_plon_s -> 
  return $ OneArgFunctionApplications plon ad_plon_s
  :: Parser OneArgFunctionApplications

ad_plon_p = 
  application_direction_p >>= \ad ->
  paren_lit_or_name_p >>= \plon ->
  return ( ad, plon )
  :: Parser ( ApplicationDirection, ParenLitOrName )

-- MultiplicationFactor

multiplication_factor_p =
  OneArgApplicationsMF <$> try one_arg_function_applications_p <|>
  ParenLitOrNameMF <$> paren_lit_or_name_p
  :: Parser MultiplicationFactor

-- Multiplication

multiplication_p = Multiplication <$> seperated2 multiplication_factor_p " * "
  :: Parser Multiplication

-- SubtractionFactor

subtraction_factor_p =
  MultiplicationSF <$> try multiplication_p <|>
  OneArgApplicationsSF <$> try one_arg_function_applications_p <|>
  ParenLitOrNameSF <$> paren_lit_or_name_p 
  :: Parser SubtractionFactor

-- Subtraction

subtraction_p =
  subtraction_factor_p >>= \sf1 ->
  string " - " >> subtraction_factor_p >>= \sf2 ->
  return $ Subtraction sf1 sf2
  :: Parser Subtraction

-- NoAbstractionsValue1

no_abstractions_value_1_p =
  Sub <$> try subtraction_p <|> Mul <$> try multiplication_p <|>
  OneArgApps <$> try one_arg_function_applications_p <|> PLON <$> paren_lit_or_name_p
  :: Parser NoAbstractionsValue1

-- ManyArgsAppArgValue

many_args_app_arg_value_p = try many_args_app_arg_value2_p <|> many_args_app_arg_value1_p
  :: Parser ManyArgsAppArgValue

many_args_app_arg_value1_p =
  abstractions_p >>= \as ->
  no_abstractions_value_1_p >>= \nae1 ->
  return $ ManyArgsAppArgValue as nae1
  :: Parser ManyArgsAppArgValue

many_args_app_arg_value2_p =
  comma_seperated2 abstraction_p >>= \as1 ->
  string " :> " >>
  many_args_app_arg_value1_p >>= \(ManyArgsAppArgValue (Abstractions as2) nav1) ->
  return $ ManyArgsAppArgValue (Abstractions $ as1 ++ as2) nav1
  :: Parser ManyArgsAppArgValue

-- ManyArgsApplication

many_args_application_p =
  comma_seperated2 many_args_app_arg_value_p >>= \maaavs ->
  string " :-> " >> value_name_p >>= \vn ->
  return $ ManyArgsApplication maaavs vn
  :: Parser ManyArgsApplication

-- SpecificCase

specific_case_p =
  spaces_tabs >> literal_or_value_name_p >>= \lovn ->
  string " ->" >> (try new_line_space_surrounded <|> (char ' ')) >>
  value_expression_p >>= \v ->
  return $ SpecificCase lovn v
  :: Parser SpecificCase

-- Cases

cases_p = fmap Cases_ $ string "cases\n" *> many1 (specific_case_p <* char '\n')
  :: Parser Cases

-- NameTypeAndValue

name_type_and_value_p =
  value_name_p >>= \vn ->
  string ": " >> value_type_p >>= \vt ->
  new_line_space_surrounded >> string "= " >> value_expression_p >>= \v ->
  (eof <|> skipMany1 new_line_space_surrounded) >> NameTypeAndValue vn vt v-->return
  :: Parser NameTypeAndValue

-- NameTypeAndValueLists

name_type_and_value_lists_p = 
  spaces_tabs >> comma_seperated2 value_name_p >>= \vns ->
  string ": " >> comma_seperated2 value_type_p >>= \vts ->
  new_line_space_surrounded >>
  string "= " >> comma_seperated2 value_expression_p >>= \vs ->
  (eof <|> skipMany1 new_line_space_surrounded) >>
  NameTypeAndValueLists vns vts vs-->return
  :: Parser NameTypeAndValueLists
  
-- NTAVOrNTAVLists

ntav_or_ntav_lists_p = 
  NTAVLists <$> try name_type_and_value_lists_p <|> NTAV <$> name_type_and_value_p
  :: Parser NTAVOrNTAVLists

-- NamesTypesAndValues

names_types_and_values_p = NamesTypesAndValues <$> try ntav_or_ntav_lists_p-->many1
  :: Parser NamesTypesAndValues

-- IntermediatesOutput

intermediates_output_p = 
  spaces_tabs >> string "intermediates" >> new_line_space_surrounded >>
  names_types_and_values_p >>= \ns_ts_and_vs ->
  spaces_tabs >> string "output" >> new_line_space_surrounded >>
  value_expression_p >>= \v ->
  return $ IntermediatesOutput_ ns_ts_and_vs v
  :: Parser IntermediatesOutput

-- NoAbstractionsValue

no_abstraction_expression_p =
  ManyArgsApp <$> try many_args_application_p <|> Cases <$> try cases_p <|>
  IntermediatesOutput <$> try intermediates_output_p <|>
  NoAbstractionsValue1 <$> no_abstractions_value_1_p 
  :: Parser NoAbstractionsValue

-- Value

value_expression_p = try value_expression2_p <|> value_expression1_p
  :: Parser Value

value_expression1_p =
  abstractions_p >>= \as ->
  no_abstraction_expression_p >>= \nae ->
  return $ Value as nae
  :: Parser Value

value_expression2_p =
  comma_seperated2 abstraction_p >>= \as1 ->
  string " :> " >> value_expression1_p >>= \(Value (Abstractions as2) nav) ->
  return $ Value (Abstractions $ as1 ++ as2) nav
  :: Parser Value
