module Parsers.TypeDefinitions where

import Text.Parsec
import Text.Parsec.String (Parser)

import Helpers ((==>), eof_or_spicy_nls)

import ParsingTypes.TypeDefinitions

import Parsers.LowLevel (value_name_p)
import Parsers.Types (type_name_p, value_type_p)

-- All:
-- Field, TupleTypeDef, OrTypeCase, OrTypeDef, TypeDefinition

-- ManyTNamesInParen: many_type_names_in_parenthesis_p

many_type_names_in_parenthesis_p =
  string "(" >> type_name_p >>= \type_name1 ->
  string ", " >> type_name_p >>= \type_name2 ->
  many (string ", " >> type_name_p) >>= \type_names ->
  string ")" >>
  return (ParenTypeNames type_name1 type_name2 type_names)
  :: Parser ManyTNamesInParen

-- LeftTypeVars:
-- left_type_vars_p, some_left_type_vars_p, many_left_type_vars_p

left_type_vars_p =
  option NoLeftTypeVars $ try some_left_type_vars_p
  :: Parser LeftTypeVars

some_left_type_vars_p =
  ( ManyLeftTypeVars <$> many_type_names_in_parenthesis_p <|>
    OneLeftTypeVar <$> type_name_p
  ) <* string "==>"
  :: Parser LeftTypeVars

-- RightTypeVars:
-- right_type_vars_p, some_right_type_vars_p, many_right_type_vars_p

right_type_vars_p =
  option NoRightTypeVars some_right_type_vars_p
  :: Parser RightTypeVars

some_right_type_vars_p =
  string "<==" *>
  ( ManyRightTypeVars <$> many_type_names_in_parenthesis_p <|>
    OneRightTypeVar <$> type_name_p
  )
  :: Parser RightTypeVars

-- TypeConsAndVars: cons_and_type_vars_p

cons_and_type_vars_p = 
  left_type_vars_p >>= \left_type_vars ->
  type_name_p >>= \type_name ->
  right_type_vars_p >>= \right_type_vars ->
  return $ TypeConsAndVars type_name left_type_vars right_type_vars
  :: Parser TypeConsAndVars

-- Field: field_name_and_type_p

field_name_and_type_p = 
  value_name_p >>= \value_name -> string ": " >> value_type_p >>= \value_type ->
  return $ NameAndType value_name value_type
  :: Parser Field

-- TupleTypeDef: tuple_type_definition_p

tuple_type_definition_p =
  string "tuple_type " >> cons_and_type_vars_p >>= \cons_and_type_vars ->
  string "\nvalue (" >>
  (field_name_and_type_p==>sepBy $ string ", ") >>= \fields ->
  string ")" >>
  return (ConsVarsAndFields cons_and_type_vars fields)
  :: Parser TupleTypeDef

-- OrTypeCase: case_and_maybe_type_p

case_and_maybe_type_p = 
  value_name_p >>= \value_name ->
  optionMaybe (string "<==(value: " *> value_type_p <* char ')') >>=
    \maybe_value_type ->
  return $ NameAndMaybeInT value_name maybe_value_type
  :: Parser OrTypeCase

-- OrTypeDef: or_type_definition_p

or_type_definition_p =
  string "or_type " >> cons_and_type_vars_p >>= \cons_and_type_vars ->
  string "\nvalues " >> case_and_maybe_type_p >>= \case1 ->
  string " | " >> case_and_maybe_type_p >>= \case2 ->
  many (try $ string " | " >> case_and_maybe_type_p) >>= \cases ->
  return (ConsVarsAndCases cons_and_type_vars case1 case2 cases)
  :: Parser OrTypeDef

-- TypeDefinition: type_definition_p

type_definition_p = 
  TupleTypeDef <$> tuple_type_definition_p <|>
  OrTypeDef <$> or_type_definition_p
  :: Parser TypeDefinition
