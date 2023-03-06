{-# language LambdaCase #-}

module Parsers.Types where

import Text.Parsec
  ( (<|>), many, char, lower, upper, string, sepBy, try, optionMaybe )
import Text.Parsec.String
  ( Parser )

import Helpers
  ( (==>), seperated2, eof_or_new_lines )

import HaskellTypes.Types
  ( TypeName(..), CartesianProduct(..), Output(..), MultipleInputs(..), Input(..)
  , FuncType(..), ValueType(..)
  , FieldAndType(..), TupleTypeDef(..), CaseAndMaybeType(..), OrTypeDef(..)
  , TypeDef(..) )

import Parsers.LowLevel
  ( value_name_p )

-- All:
-- type_name_p, tuple_type_p, base_type_p, value_type_p, field_and_type_p,
-- tuple_type_def_p, case_and_maybe_type_p, or_type_def_p, type_def_p

type_name_p =
  upper >>= \initial_upper -> many (lower <|> upper) >>= \lowers_uppers ->
  return $ TN (initial_upper : lowers_uppers)
  :: Parser TypeName

cartesian_product_p =
  inner_value_type_p >>= \vt1 ->
  string " x " >> inner_value_type_p >>= \vt2 ->
  many (try $ string " x " >> inner_value_type_p) >>= \vts ->
  return $ Types vt1 vt2 vts
  :: Parser CartesianProduct

inner_value_type_p =
  try (char '(' *>
  (FuncType <$> try func_type_p <|> CartesianProduct <$> cartesian_product_p)
  <* char ')') <|>
  TypeName <$> type_name_p
  :: Parser ValueType

output_p =
  OutputCartesianProduct <$> try cartesian_product_p <|>
  OutputTypeName <$> type_name_p
  :: Parser Output

multiple_inputs_p =
  char '(' >> value_type_p >>= \vt1 ->
  string ", " >> value_type_p >>= \vt2 ->
  many (string ", " >> value_type_p) >>= \vts ->
  char ')' >> return (InputTypes vt1 vt2 vts)
  :: Parser MultipleInputs

input_p = 
  MultipleInputs <$> try multiple_inputs_p <|>
  OneInput <$> inner_value_type_p
  :: Parser Input

func_type_p =
  input_p >>= \input -> string " -> " >> output_p >>= \output ->
  return $ InputAndOutput input output
  :: Parser FuncType

value_type_p =
  FuncType <$> try func_type_p <|> CartesianProduct <$> try cartesian_product_p <|>
  TypeName <$> type_name_p
  :: Parser ValueType

field_and_type_p = 
  value_name_p >>= \vn -> string ": " >> value_type_p >>= \vt ->
  return $ FT vn vt
  :: Parser FieldAndType

tuple_type_def_p =
  string "tuple_type " >> type_name_p >>= \tn ->
  string "\nvalue (" >> (field_and_type_p==>sepBy $ string ", ") >>= \ttv ->
  string ")" >> eof_or_new_lines >> NameAndValue tn ttv==>return
  :: Parser TupleTypeDef

case_and_maybe_type_p = 
  value_name_p >>= \vn -> optionMaybe (char '.' *> value_type_p) >>= \mvt ->
  return $ CMT vn mvt
  :: Parser CaseAndMaybeType

or_type_def_p =
  string "or_type " >> type_name_p >>= \tn ->
  string "\nvalues " >>
  (case_and_maybe_type_p==>sepBy $ try $ string " | ") >>= \otvs ->
  eof_or_new_lines >> NameAndValues tn otvs==>return
  :: Parser OrTypeDef

type_def_p = 
  TupleTypeDef <$> tuple_type_def_p <|> OrTypeDef <$> or_type_def_p
  :: Parser TypeDef
