module Parsing.Parsers.Types where

import Text.Parsec
import Text.Parsec.String (Parser)
import Parsing.Types.Types

-- All:
-- TypeName, ProductType, InputTypeOrTypes, ManyTypesInParen, OutputType,
-- FunctionType,
-- LeftTInputs, RightTInputs, TypeApplication, ValueType, Helpers

-- TypeName: type_name_p

type_name_p =
  upper >>= \initial_upper -> many (lower <|> upper) >>= \lowers_uppers ->
  return $ TN (initial_upper : lowers_uppers)
  :: Parser TypeName

-- ProductType: product_type_p, product_value_type_p

product_type_p =
  product_value_type_p >>= \value_type1 ->
  string " x " >> product_value_type_p >>= \value_type2 ->
  many (try $ string " x " >> product_value_type_p) >>= \value_types ->
  return $ ProductTypes value_type1 value_type2 value_types
  :: Parser ProductType

product_value_type_p =
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

-- ManyTypesInParen: many_types_in_parenthesis_p

many_types_in_parenthesis_p =
  string "(" >> value_type_p >>= \value_type1 ->
  string ", " >> value_type_p >>= \value_type2 ->
  many (string ", " >> value_type_p) >>= \value_types ->
  string ")" >>
  return (TypesInParen value_type1 value_type2 value_types)
  :: Parser ManyTypesInParen

-- OutputType: output_type_p

output_type_p =
  OutputProductType <$> try product_type_p <|>
  OutputTypeApp <$> type_application_p
  :: Parser OutputType

-- FunctionType: function_type_p

function_type_p =
  input_type_or_types_p >>= \input -> string " -> " >> output_type_p >>= \output ->
  return $ InAndOutTypes input output
  :: Parser FunctionType

-- LeftTInputs: left_type_inputs_p, some_left_type_inputs_p

left_type_inputs_p =
  option NoLeftTInputs $ try some_left_type_inputs_p
  :: Parser LeftTInputs

some_left_type_inputs_p =
  ( try (ManyLeftTInputs <$> many_types_in_parenthesis_p) <|>
    OneLeftTInput <$> one_type_input_p
  )
  <* string "==>"
  :: Parser LeftTInputs

-- RightTInputs: right_type_inputs_p, some_right_type_inputs_p

right_type_inputs_p =
  option NoRightTInputs $ try some_right_type_inputs_p
  :: Parser RightTInputs

some_right_type_inputs_p =
  string "<==" *>
  ( try (ManyRightTInputs <$> many_types_in_parenthesis_p) <|>
    OneRightTInput <$> one_type_input_p
  )
  :: Parser RightTInputs

-- TypeApplication: type_application_p

type_application_p = 
  left_type_inputs_p >>= \left_type_inputs ->
  type_name_p >>= \type_name ->
  right_type_inputs_p >>= \right_type_inputs ->
  return $ TypeConsAndInputs type_name left_type_inputs right_type_inputs
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
    TypeConsAndInputs type_name NoLeftTInputs NoRightTInputs
  ) :: TypeName -> ValueType
