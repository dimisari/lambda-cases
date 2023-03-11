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
  , FunctionType(..), ValueType(..)
  , FieldAndType(..), TupleTypeDef(..), CaseAndMaybeType(..), OrTypeDef(..)
  , TypeDef(..) )

import Parsers.LowLevel
  ( value_name_p )

-- All:
-- TypeName, CartesianProduct, Output, MultipleInputs, Input, FunctionType, ValueType,
-- FieldAndType, TupleTypeDef, CaseAndMaybeType, OrTypeDef, TypeDef, FieldsOrCases

-- TypeName: type_name_p
 
type_name_p =
  upper >>= \initial_upper -> many (lower <|> upper) >>= \lowers_uppers ->
  return $ TN (initial_upper : lowers_uppers)
  :: Parser TypeName

-- CartesianProduct: cartesian_product_p, inner_value_type_p

cartesian_product_p =
  inner_value_type_p >>= \vt1 ->
  string " x " >> inner_value_type_p >>= \vt2 ->
  many (try $ string " x " >> inner_value_type_p) >>= \vts ->
  return $ Types vt1 vt2 vts
  :: Parser CartesianProduct

inner_value_type_p =
  try (char '(' *>
  (FunctionType <$> try func_type_p <|> CartesianProduct <$> cartesian_product_p)
  <* char ')') <|>
  TypeName <$> type_name_p
  :: Parser ValueType

-- Output: output_p

output_p =
  OutputCartesianProduct <$> try cartesian_product_p <|>
  OutputTypeName <$> type_name_p
  :: Parser Output

-- MultipleInputs: multiple_inputs_p

multiple_inputs_p =
  char '(' >> value_type_p >>= \vt1 ->
  string ", " >> value_type_p >>= \vt2 ->
  many (string ", " >> value_type_p) >>= \vts ->
  char ')' >> return (InputTypes vt1 vt2 vts)
  :: Parser MultipleInputs

-- Input: input_p, one_input_val_type_p

input_p = 
  MultipleInputs <$> try multiple_inputs_p <|>
  OneInput <$> one_input_val_type_p
  :: Parser Input

one_input_val_type_p =
  FunctionType <$> (char '(' *> func_type_p <* char ')') <|>
  CartesianProduct <$> try cartesian_product_p <|>
  TypeName <$> type_name_p
  :: Parser ValueType

-- FunctionType: func_type_p

func_type_p =
  input_p >>= \input -> string " -> " >> output_p >>= \output ->
  return $ InputAndOutput input output
  :: Parser FunctionType

-- ValueType: value_type_p

value_type_p =
  FunctionType <$> try func_type_p <|> CartesianProduct <$> try cartesian_product_p <|>
  TypeName <$> type_name_p
  :: Parser ValueType

-- FieldAndType: field_and_type_p

field_and_type_p = 
  value_name_p >>= \vn -> string ": " >> value_type_p >>= \vt ->
  return $ FT vn vt
  :: Parser FieldAndType

-- TupleTypeDef: tuple_type_def_p

tuple_type_def_p =
  string "tuple_type " >> type_name_p >>= \tn ->
  string "\nvalue (" >> (field_and_type_p==>sepBy $ string ", ") >>= \ttv ->
  string ")" >> eof_or_new_lines >> NameAndValue tn ttv==>return
  :: Parser TupleTypeDef

-- CaseAndMaybeType: case_and_maybe_type_p

case_and_maybe_type_p = 
  value_name_p >>= \vn -> optionMaybe (char '.' *> value_type_p) >>= \mvt ->
  return $ CMT vn mvt
  :: Parser CaseAndMaybeType

-- OrTypeDef: or_type_def_p

or_type_def_p =
  string "or_type " >> type_name_p >>= \tn ->
  string "\nvalues " >>
  (case_and_maybe_type_p==>sepBy $ try $ string " | ") >>= \otvs ->
  eof_or_new_lines >> NameAndValues tn otvs==>return
  :: Parser OrTypeDef

-- TypeDef: type_def_p

type_def_p = 
  TupleTypeDef <$> tuple_type_def_p <|> OrTypeDef <$> or_type_def_p
  :: Parser TypeDef
