{-# language LambdaCase #-}

module Parsers.Types where

import Text.Parsec
  ( (<|>), many, char, lower, upper, string, sepBy, try, optionMaybe )
import Text.Parsec.String
  ( Parser )

import Helpers
  ( (==>), seperated2, eof_or_new_lines )

import HaskellTypes.Types
  ( TypeName(..), ParenType(..), BaseType(..), ValueType(..), FieldAndType(..)
  , TupleTypeDef(..), CaseAndMaybeType(..), OrTypeDef(..), TypeDef(..) )

import Parsers.LowLevel
  ( value_name_p )

-- All:
-- type_name_p, ParenType, base_type_p, ValueType, field_and_type_p,
-- tuple_type_def_p, case_and_maybe_type_p, or_type_def_p, type_def_p

type_name_p =
  upper >>= \u -> many (lower <|> upper) >>= \lu -> return $ TN (u:lu)
  :: Parser TypeName

-- ParenType: paren_type_p, tuple_type_p
paren_type_p =
  char '(' *> (tuple_type_p <|> ParenVT <$> value_type_p) <* char ')'
  :: Parser ParenType

tuple_type_p =
  char ' ' >> value_type_p >>= \vt1 ->
  string ", " >> value_type_p >>= \vt2 ->
  many (string ", " >> value_type_p) >>= \vts ->
  char ' ' >> return (TupleType vt1 vt2 vts)
  :: Parser ParenType
-- ParenType end

base_type_p =
  ParenType <$> paren_type_p <|> TypeName <$> type_name_p 
  :: Parser BaseType

-- ValueType: value_type_p, many_abstractions_arrow_p, one_abstraction_arrow_p
value_type_p =
  try many_abstractions_arrow_p <|> one_abstraction_arrow_p
  :: Parser ValueType

many_abstractions_arrow_p =
  seperated2 ", " one_abstraction_arrow_p >>= \vt1s ->
  string " *-> " >> one_abstraction_arrow_p >>= \(AbsTypesAndResType bts bt) ->
  let
  vt_to_bt = \case
    AbsTypesAndResType [] bt -> bt
    vt -> ParenType $ ParenVT vt
    :: ValueType -> BaseType
  in
  return $ AbsTypesAndResType (map vt_to_bt vt1s ++ bts) bt
  :: Parser ValueType

one_abstraction_arrow_p =
  many (try $ base_type_p <* string " -> ") >>= \bts -> base_type_p >>= \bt ->
  return $ AbsTypesAndResType bts bt
  :: Parser ValueType
-- ValueType end

field_and_type_p = 
  value_name_p >>= \vn -> string ": " >> value_type_p >>= \vt ->
  return $ FT vn vt
  :: Parser FieldAndType

tuple_type_def_p =
  string "tuple_type " >> type_name_p >>= \tn ->
  string "\nvalue ( " >> (field_and_type_p==>sepBy $ string ", ") >>= \ttv ->
  string " )" >> eof_or_new_lines >> NameAndValue tn ttv==>return
  :: Parser TupleTypeDef

case_and_maybe_type_p = 
  value_name_p >>= \vn -> optionMaybe (char '.' *> value_type_p) >>= \mvt ->
  return $ CT vn mvt
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
