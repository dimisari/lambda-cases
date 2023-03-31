module Parsers.LowLevelTypes where

import Text.Parsec
import Text.Parsec.String (Parser)

import ParsingTypes.LowLevelTypes 

-- All: TypeName, LeftTypeVars, RightTypeVars, ConsAndTypeVars

-- TypeName: type_name_p
 
type_name_p =
  upper >>= \initial_upper -> many (lower <|> upper) >>= \lowers_uppers ->
  return $ TN (initial_upper : lowers_uppers)
  :: Parser TypeName

-- LeftTypeVars: left_type_vars_p, some_left_type_vars_p, many_left_type_vars_p

left_type_vars_p =
  option NoLeftTVars $ try some_left_type_vars_p
  :: Parser LeftTypeVars

some_left_type_vars_p =
  (many_left_type_vars_p <|> OneLeftTVar <$> type_name_p) <* string "==>"
  :: Parser LeftTypeVars

many_left_type_vars_p = 
  many_type_vars_p >>= \(type_name1, type_name2, type_names) ->
  return $ ManyLeftTVars type_name1 type_name2 type_names
  :: Parser LeftTypeVars

-- RightTypeVars: right_type_vars_p, some_right_type_vars_p, many_right_type_vars_p

right_type_vars_p =
  option NoRightTVar some_right_type_vars_p
  :: Parser RightTypeVars

some_right_type_vars_p =
  string "<==" *> (many_right_type_vars_p <|> OneRightTVar <$> type_name_p)
  :: Parser RightTypeVars

many_right_type_vars_p = 
  many_type_vars_p >>= \(type_name1, type_name2, type_names) ->
  return $ ManyRightTVars type_name1 type_name2 type_names
  :: Parser RightTypeVars

-- ConsAndTypeVars: cons_and_type_vars_p

cons_and_type_vars_p = 
  left_type_vars_p >>= \left_input_ts ->
  type_name_p >>= \type_name ->
  right_type_vars_p >>= \right_input_ts ->
  return $ ConsAndTVars type_name left_input_ts right_input_ts
  :: Parser ConsAndTypeVars

-- Helpers: many_type_vars_p

many_type_vars_p =
  string "(" >> type_name_p >>= \type_name1 ->
  string ", " >> type_name_p >>= \type_name2 ->
  many (string ", " >> type_name_p) >>= \type_names ->
  string ")" >>
  return (type_name1, type_name2, type_names)
  :: Parser (TypeName, TypeName, [ TypeName ])

