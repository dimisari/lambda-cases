module Parsers.Types where

import Text.Parsec
import Text.Parsec.String (Parser)

import Helpers ((==>), eof_or_new_lines)

import ParsingTypes.Types

import Parsers.LowLevel (value_name_p)

-- All:
-- TypeName, ProductType, InputTypeOrTypes, ManyTypesInParenthesis, OutputType,
-- FunctionType,
-- LeftTypeInputs, RightTypeInputs, TypeApplication, ValueType

-- TypeName: type_name_p

type_name_p =
  upper >>= \initial_upper -> many (lower <|> upper) >>= \lowers_uppers ->
  return $ TN (initial_upper : lowers_uppers)
  :: Parser TypeName

-- ProductType: product_type_p, inner_value_type_p

product_type_p =
  inner_value_type_p >>= \value_type1 ->
  string " x " >> inner_value_type_p >>= \value_type2 ->
  many (try $ string " x " >> inner_value_type_p) >>= \value_types ->
  return $ ProductTypes value_type1 value_type2 value_types
  :: Parser ProductType

inner_value_type_p =
  try (char '(' *>
  (FunctionType <$> try function_type_p <|> ProductType <$> product_type_p)
  <* char ')') <|>
  TypeApplication <$> type_application_p
  :: Parser ValueType

-- InputTypeOrTypes: input_type_or_types_p, one_input_val_type_p

input_type_or_types_p = 
  MultipleInputTypes <$> try many_types_in_parenthesis_p <|>
  OneInputType <$> one_input_val_type_p
  :: Parser InputTypeOrTypes

one_input_val_type_p =
  FunctionType <$> try (char '(' *> function_type_p <* char ')') <|>
  ProductType <$> try product_type_p <|>
  TypeApplication <$> type_application_p
  :: Parser ValueType

-- ManyTypesInParenthesis: many_types_in_parenthesis_p

many_types_in_parenthesis_p =
  string "(" >> value_type_p >>= \value_type1 ->
  string ", " >> value_type_p >>= \value_type2 ->
  many (string ", " >> value_type_p) >>= \value_types ->
  string ")" >>
  return (TypesInParenthesis value_type1 value_type2 value_types)
  :: Parser ManyTypesInParenthesis

-- OutputType: output_type_p

output_type_p =
  OutputProductType <$> try product_type_p <|>
  OutputTypeApplication <$> type_application_p
  :: Parser OutputType

-- FunctionType: function_type_p

function_type_p =
  input_type_or_types_p >>= \input -> string " -> " >> output_type_p >>= \output ->
  return $ InputAndOutputTypes input output
  :: Parser FunctionType

-- LeftTypeInputs:
-- left_type_inputs_p, some_left_type_inputs_p

left_type_inputs_p =
  option NoLeftTypeInputs $ try some_left_type_inputs_p
  :: Parser LeftTypeInputs

some_left_type_inputs_p =
  ( try (ManyLeftTypeInputs <$> many_types_in_parenthesis_p) <|>
    OneLeftTypeInput <$> one_type_input_p
  )
  <* string "==>"
  :: Parser LeftTypeInputs

-- RightTypeInputs:
-- right_type_inputs_p, some_right_type_inputs_p, many_right_type_inputs_p

right_type_inputs_p =
  option NoRightTypeInputs $ try some_right_type_inputs_p
  :: Parser RightTypeInputs

some_right_type_inputs_p =
  string "<==" *>
  ( try (ManyRightTypeInputs <$> many_types_in_parenthesis_p) <|>
    OneRightTypeInput <$> one_type_input_p
  )
  :: Parser RightTypeInputs

-- TypeApplication: type_application_p

type_application_p = 
  left_type_inputs_p >>= \left_type_inputs ->
  type_name_p >>= \type_name ->
  right_type_inputs_p >>= \right_type_inputs ->
  return $ TypeConstructorAndInputs type_name left_type_inputs right_type_inputs
  :: Parser TypeApplication

-- ValueType: value_type_p

value_type_p =
  FunctionType <$> try function_type_p <|> ProductType <$> try product_type_p <|>
  TypeApplication <$> type_application_p
  :: Parser ValueType

-- Helpers: 

one_type_input_p =
  (char '(' *> value_type_p <* char ')') <|>
  type_name_to_value_type <$> type_name_p

type_name_to_value_type = ( \type_name ->
  TypeApplication $
    TypeConstructorAndInputs type_name NoLeftTypeInputs NoRightTypeInputs
  ) :: TypeName -> ValueType
