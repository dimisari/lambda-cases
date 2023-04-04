module Parsers.TypeDefinitions where

import Text.Parsec
import Text.Parsec.String (Parser)

import Helpers ((==>), eof_or_new_lines)

import ParsingTypes.Types (TypeName(..))
import ParsingTypes.TypeDefinitions

import Parsers.LowLevel (value_name_p)
import Parsers.Types (type_name_p, value_type_p)

-- All:
-- Field, TupleTypeDefinition, OrTypeCase, OrTypeDefinition, TypeDefinition

-- ManyTypeNamesInParenthesis: many_type_names_in_parenthesis_p

many_type_names_in_parenthesis_p =
  string "(" >> type_name_p >>= \type_name1 ->
  string ", " >> type_name_p >>= \type_name2 ->
  many (string ", " >> type_name_p) >>= \type_names ->
  string ")" >>
  return (ParenTypeNames type_name1 type_name2 type_names)
  :: Parser ManyTypeNamesInParenthesis

-- LeftTypeVariables:
-- left_type_vars_p, some_left_type_vars_p, many_left_type_vars_p

left_type_vars_p =
  option NoLeftTypeVariables $ try some_left_type_vars_p
  :: Parser LeftTypeVariables

some_left_type_vars_p =
  ( ManyLeftTypeVariables <$> many_type_names_in_parenthesis_p <|>
    OneLeftTypeVariable <$> type_name_p
  ) <* string "==>"
  :: Parser LeftTypeVariables

-- RightTypeVariables:
-- right_type_vars_p, some_right_type_vars_p, many_right_type_vars_p

right_type_vars_p =
  option NoRightTypeVariables some_right_type_vars_p
  :: Parser RightTypeVariables

some_right_type_vars_p =
  string "<==" *>
  ( ManyRightTypeVariables <$> many_type_names_in_parenthesis_p <|>
    OneRightTypeVariable <$> type_name_p
  )
  :: Parser RightTypeVariables

-- TypeConstructorAndVariables: cons_and_type_vars_p

cons_and_type_vars_p = 
  left_type_vars_p >>= \left_input_ts ->
  type_name_p >>= \type_name ->
  right_type_vars_p >>= \right_input_ts ->
  return $ TypeConstructorAndVariables type_name left_input_ts right_input_ts
  :: Parser TypeConstructorAndVariables

-- Field: field_name_and_type_p

field_name_and_type_p = 
  value_name_p >>= \value_name -> string ": " >> value_type_p >>= \value_type ->
  return $ NameAndType value_name value_type
  :: Parser Field

-- TupleTypeDefinition: tuple_type_definition_p

tuple_type_definition_p =
  string "tuple_type " >> cons_and_type_vars_p >>= \type_application ->
  string "\nvalue (" >>
  (field_name_and_type_p==>sepBy $ string ", ") >>= \fields ->
  string ")" >> eof_or_new_lines >>
  ConstructorAndFields type_application fields==>return
  :: Parser TupleTypeDefinition

-- OrTypeCase: case_and_maybe_type_p

case_and_maybe_type_p = 
  value_name_p >>= \value_name ->
  optionMaybe (string "<==(value: " *> value_type_p <* char ')') >>=
    \maybe_value_type ->
  return $ NameAndMaybeInputType value_name maybe_value_type
  :: Parser OrTypeCase

-- OrTypeDefinition: or_type_definition_p

or_type_definition_p =
  string "or_type " >> cons_and_type_vars_p >>= \type_application ->
  string "\nvalues " >> case_and_maybe_type_p >>= \case1 ->
  string " | " >> case_and_maybe_type_p >>= \case2 ->
  many (try $ string " | " >> case_and_maybe_type_p) >>= \cases ->
  eof_or_new_lines >>
  ConstructorAndCases type_application case1 case2 cases==>return
  :: Parser OrTypeDefinition

-- TypeDefinition: type_definition_p

type_definition_p = 
  TupleTypeDefinition <$> tuple_type_definition_p <|>
  OrTypeDefinition <$> or_type_definition_p
  :: Parser TypeDefinition
