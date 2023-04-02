module Parsers.Types where

import Text.Parsec
import Text.Parsec.String (Parser)

import Helpers ((==>), eof_or_new_lines)

import ParsingTypes.LowLevelTypes (TypeName)
import ParsingTypes.Types

import Parsers.LowLevelValues (value_name_p)
import Parsers.LowLevelTypes (type_name_p)

-- All:
-- ProductType, InputTypeOrTypes, InputTypes, OutputType, FunctionType,
-- LeftTypeInputs, RightTypeInputs, TypeApplication, ValueType

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
  TypeApplication <$> type_application_p
  :: Parser ValueType

-- InputTypeOrTypes: input_type_or_types_p, one_input_val_type_p

input_type_or_types_p = 
  MultipleInputs <$> try input_types_p <|> OneInput <$> one_input_val_type_p
  :: Parser InputTypeOrTypes

one_input_val_type_p =
  FunctionType <$> try (char '(' *> function_type_p <* char ')') <|>
  ProductType <$> try product_type_p <|>
  TypeApplication <$> type_application_p
  :: Parser ValueType

-- InputTypes: input_types_p

input_types_p =
  value_type_tuple_p >>= \(value_type1, value_type2, value_types) ->
  return (InputTypes value_type1 value_type2 value_types)
  :: Parser InputTypes

-- OutputType: output_type_p

output_type_p =
  OutputProductType <$> try product_type_p <|>
  OutputTypeApp <$> type_application_p
  :: Parser OutputType

-- FunctionType: function_type_p

function_type_p =
  input_type_or_types_p >>= \input -> string " -> " >> output_type_p >>= \output ->
  return $ InputAndOutputTypes input output
  :: Parser FunctionType

-- LeftTypeInputs:
-- left_type_inputs_p, some_left_type_inputs_p, many_left_type_inputs_p

left_type_inputs_p =
  option NoLeftTypeInputs $ try some_left_type_inputs_p
  :: Parser LeftTypeInputs

some_left_type_inputs_p =
  (try many_left_type_inputs_p <|> OneLeftTypeInput <$> one_type_input_p)
  <* string "==>"
  :: Parser LeftTypeInputs

many_left_type_inputs_p = 
  value_type_tuple_p >>= \(input_t1, input_t2, type_inputs) ->
  return $ ManyLeftTypeInputs input_t1 input_t2 type_inputs
  :: Parser LeftTypeInputs

-- RightTypeInputs:
-- right_type_inputs_p, some_right_type_inputs_p, many_right_type_inputs_p

right_type_inputs_p =
  option NoRightTypeInputs $ try some_right_type_inputs_p
  :: Parser RightTypeInputs

some_right_type_inputs_p =
  string "<==" *>
  (try many_right_type_inputs_p <|> OneRightTypeInput <$> one_type_input_p)
  :: Parser RightTypeInputs

many_right_type_inputs_p = 
  value_type_tuple_p >>= \(input_t1, input_t2, type_inputs) ->
  return $ ManyRightTypeInputs input_t1 input_t2 type_inputs
  :: Parser RightTypeInputs

-- TypeApplication: type_application_p

type_application_p = 
  left_type_inputs_p >>= \left_type_inputs ->
  type_name_p >>= \type_name ->
  right_type_inputs_p >>= \right_type_inputs ->
  return $ ConsAndTypeInputs type_name left_type_inputs right_type_inputs
  :: Parser TypeApplication

-- ValueType: value_type_p

value_type_p =
  FunctionType <$> try function_type_p <|> ProductType <$> try product_type_p <|>
  TypeApplication <$> type_application_p
  :: Parser ValueType

-- Helpers: value_type_tuple_p

value_type_tuple_p =
  string "(" >> value_type_p >>= \value_type1 ->
  string ", " >> value_type_p >>= \value_type2 ->
  many (string ", " >> value_type_p) >>= \value_types ->
  string ")" >>
  return (value_type1, value_type2, value_types)
  :: Parser (ValueType, ValueType, [ ValueType ])

one_type_input_p =
  (char '(' *> value_type_p <* char ')') <|>
  type_name_to_value_type <$> type_name_p

type_name_to_value_type = ( \type_name ->
  TypeApplication $
    ConsAndTypeInputs type_name NoLeftTypeInputs NoRightTypeInputs
  ) :: TypeName -> ValueType
