module Parsers.LowLevelTypes where

import Text.Parsec
import Text.Parsec.String (Parser)

import ParsingTypes.LowLevelTypes 

-- All: TypeName, LeftTypeInputs, RightTypeInputs, TypeApplication

-- TypeName: type_name_p
 
type_name_p =
  upper >>= \initial_upper -> many (lower <|> upper) >>= \lowers_uppers ->
  return $ TN (initial_upper : lowers_uppers)
  :: Parser TypeName

-- LeftTypeInputs:

left_type_inputs_p =
  option (LeftTypeInputs []) non_empty_left_type_inputs_p
  :: Parser LeftTypeInputs

non_empty_left_type_inputs_p =
  string "(" >> type_name_p >>= \type_name1 ->
  many (string ", " >> type_name_p) >>= \type_names ->
  string ")==>" >>
  return (LeftTypeInputs $ type_name1 : type_names)
  :: Parser LeftTypeInputs

-- RightTypeInputs:

right_type_inputs_p =
  option (RightTypeInputs []) non_empty_right_type_inputs_p
  :: Parser RightTypeInputs

non_empty_right_type_inputs_p =
  string "<==(" >> type_name_p >>= \type_name1 ->
  many (string ", " >> type_name_p) >>= \type_names ->
  string ")" >>
  return (RightTypeInputs $ type_name1 : type_names)
  :: Parser RightTypeInputs

-- TypeApplication

type_application_p = 
  left_type_inputs_p >>= \left_type_inputs ->
  type_name_p >>= \type_name ->
  right_type_inputs_p >>= \right_type_inputs ->
  return $ NameAndTypeInputs type_name left_type_inputs right_type_inputs
  :: Parser TypeApplication
