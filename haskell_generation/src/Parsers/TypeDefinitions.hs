module Parsers.TypeDefinitions where

import Text.Parsec
import Text.Parsec.String (Parser)

import Helpers ((==>), eof_or_new_lines)

import ParsingTypes.LowLevelTypes (TypeName(..))
import ParsingTypes.TypeDefinitions

import Parsers.LowLevelValues (value_name_p)
import Parsers.LowLevelTypes (type_name_p)
import Parsers.Types (value_type_p)

-- All:
-- Field, TupleTypeDefinition, OrTypeCase, OrTypeDefinition, TypeDefinition

-- Field: field_name_and_type_p

field_name_and_type_p = 
  value_name_p >>= \value_name -> string ": " >> value_type_p >>= \value_type ->
  return $ NameAndType value_name value_type
  :: Parser Field

-- TupleTypeDefinition: tuple_type_definition_p

tuple_type_definition_p =
  string "tuple_type " >> type_name_p >>= \type_name ->
  string "\nvalue (" >>
  (field_name_and_type_p==>sepBy $ string ", ") >>= \fields ->
  string ")" >> eof_or_new_lines >> NameAndFields type_name fields==>return
  :: Parser TupleTypeDefinition

-- OrTypeCase: case_and_maybe_type_p

case_and_maybe_type_p = 
  value_name_p >>= \value_name ->
  optionMaybe (string "<==(value: " *> value_type_p <* char ')') >>=
    \maybe_value_type ->
  return $ OrTypeCase value_name maybe_value_type
  :: Parser OrTypeCase

-- OrTypeDefinition: or_type_definition_p

or_type_definition_p =
  string "or_type " >> type_name_p >>= \type_name ->
  string "\nvalues " >> case_and_maybe_type_p >>= \case1 ->
  string " | " >> case_and_maybe_type_p >>= \case2 ->
  many (try $ string " | " >> case_and_maybe_type_p) >>= \cases ->
  eof_or_new_lines >> NameAndCases type_name case1 case2 cases==>return
  :: Parser OrTypeDefinition

-- TypeDefinition: type_definition_p

type_definition_p = 
  TupleTypeDefinition <$> tuple_type_definition_p <|>
  OrTypeDefinition <$> or_type_definition_p
  :: Parser TypeDefinition
