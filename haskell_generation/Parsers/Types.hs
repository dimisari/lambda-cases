module Parsers.Types where

import Prelude
  ( (<$>), (>>=), (>>), (<*), (*>), (++), ($), return, map )
import Text.Parsec
  ( (<|>), many, char, lower, upper, string, sepBy, try )
import Text.Parsec.String
  ( Parser )

import Helpers
  ( (-->), new_line_space_surrounded, comma_seperated2, paren_comma_seperated2
  , eof_or_new_lines )

import HaskellTypes.Types
  ( TypeName(..), BaseType(..), ValueType(..), FieldAndType(..), TupleTypeValue(..)
  , TupleType(..) )

import Parsers.LowLevel
  ( value_name_p )

type_name_p =
  upper >>= \u ->
  many (lower <|> upper) >>= \lu ->
  return $ TN (u:lu)
  :: Parser TypeName

base_type_p =
  TupleType <$> try (paren_comma_seperated2 value_type_p) <|>
  ParenthesisType <$> (char '(' *> value_type_p <* char ')') <|>
  TypeName <$> type_name_p 
  :: Parser BaseType

-- ValueType
value_type_p =
  try value_type_2_p <|> value_type_1_p
  :: Parser ValueType

value_type_1_p =
  many (try $ base_type_p <* string " -> ") >>= \bts ->
  base_type_p >>= \bt ->
  return $ AbsTypesAndResType bts bt
  :: Parser ValueType

value_type_2_p =
  comma_seperated2 value_type_1_p >>= \vt1s ->
  string " :> " >> value_type_1_p >>= \(AbsTypesAndResType bts bt) ->
  return $ AbsTypesAndResType (map ParenthesisType vt1s ++ bts) bt
  :: Parser ValueType
-- ValueType end

field_and_type_p = 
  value_name_p >>= \vn ->
  string ": " >> value_type_p >>= \vt ->
  return $ FT vn vt
  :: Parser FieldAndType

tuple_value_p = 
  string "( " >> (field_and_type_p-->sepBy $ string ", ") >>= \fatl ->
  string " )" >> fatl-->FieldAndTypeList-->return
  :: Parser TupleTypeValue

tuple_type_p =
  string "tuple_type " >> type_name_p >>= \tn ->
  string "\nvalue " >> tuple_value_p >>= \tv ->
  eof_or_new_lines >> NameAndTupleValue tn tv-->return
  :: Parser TupleType
