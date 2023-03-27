module Parsers.Types where

import Text.Parsec
import Text.Parsec.String (Parser)

import Helpers ((==>), eof_or_new_lines)

import HaskellTypes.LowLevelTypes (TypeName(..))
import HaskellTypes.Types

import Parsers.LowLevel (value_name_p)
import Parsers.LowLevelTypes (type_name_p)

-- All:
-- TypeName, ProductType, InputTypeOrTypes, InputTypes, OutputType, FunctionType
-- ValueType
-- FieldNameAndType, TupleTypeDefinition, CaseAndMaybeType, OrTypeDefinition
-- TypeDefinition

-- ProductType: product_type_p, inner_value_type_p

product_type_p =
  inner_value_type_p >>= \value_type1 ->
  string " x " >> inner_value_type_p >>= \value_type2 ->
  many (try $ string " x " >> inner_value_type_p) >>= \value_types ->
  return $ Types value_type1 value_type2 value_types
  :: Parser ProductType

inner_value_type_p =
  try (char '(' *>
  (FunctionType <$> try function_type_p <|> ProductType <$> product_type_p)
  <* char ')') <|>
  TypeName <$> type_name_p
  :: Parser ValueType

-- InputTypeOrTypes: input_type_or_types_p, one_input_val_type_p

input_type_or_types_p = 
  MultipleInputs <$> try input_types_p <|> OneInput <$> one_input_val_type_p
  :: Parser InputTypeOrTypes

one_input_val_type_p =
  FunctionType <$> (char '(' *> function_type_p <* char ')') <|>
  ProductType <$> try product_type_p <|>
  TypeName <$> type_name_p
  :: Parser ValueType

-- InputTypes: input_types_p

input_types_p =
  char '(' >> value_type_p >>= \value_type1 ->
  string ", " >> value_type_p >>= \value_type2 ->
  many (string ", " >> value_type_p) >>= \value_types ->
  char ')' >> return (InTypes value_type1 value_type2 value_types)
  :: Parser InputTypes

-- OutputType: output_type_p

output_type_p =
  OutputProductType <$> try product_type_p <|>
  OutputTypeName <$> type_name_p
  :: Parser OutputType

-- FunctionType: function_type_p

function_type_p =
  input_type_or_types_p >>= \input -> string " -> " >> output_type_p >>= \output ->
  return $ InputAndOutput input output
  :: Parser FunctionType

-- ValueType: value_type_p

value_type_p =
  FunctionType <$> try function_type_p <|> ProductType <$> try product_type_p <|>
  TypeName <$> type_name_p
  :: Parser ValueType

-- FieldNameAndType: field_name_and_type_p

field_name_and_type_p = 
  value_name_p >>= \value_name -> string ": " >> value_type_p >>= \value_type ->
  return $ NameAndType value_name value_type
  :: Parser FieldNameAndType

-- TupleTypeDefinition: tuple_type_def_p

tuple_type_def_p =
  string "tuple_type " >> type_name_p >>= \type_name ->
  string "\nvalue (" >>
  (field_name_and_type_p==>sepBy $ string ", ") >>= \fields ->
  string ")" >> eof_or_new_lines >> NameAndFields type_name fields==>return
  :: Parser TupleTypeDefinition

-- CaseAndMaybeType: case_and_maybe_type_p

case_and_maybe_type_p = 
  value_name_p >>= \value_name ->
  optionMaybe (string "<==(value: " *> value_type_p <* char ')') >>=
    \maybe_value_type ->
  return $ CaseAndMaybeType value_name maybe_value_type
  :: Parser CaseAndMaybeType

-- OrTypeDefinition: or_type_def_p

or_type_def_p =
  string "or_type " >> type_name_p >>= \type_name ->
  string "\nvalues " >> case_and_maybe_type_p >>= \case1 ->
  string " | " >> case_and_maybe_type_p >>= \case2 ->
  many (try $ string " | " >> case_and_maybe_type_p) >>= \cases ->
  eof_or_new_lines >> NameAndCases type_name case1 case2 cases==>return
  :: Parser OrTypeDefinition

-- TypeDefinition: type_def_p

type_def_p = 
  TupleTypeDefinition <$> tuple_type_def_p <|> OrTypeDefinition <$> or_type_def_p
  :: Parser TypeDefinition
