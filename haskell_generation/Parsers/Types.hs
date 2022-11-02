{-# LANGUAGE LambdaCase #-}

module Parsers.Types where

import Prelude ( String, Either, Show, (>>=), (>>), (*>), (++), ($), show, return, flip )
import Text.Parsec
  ( ParseError, (<|>), parse, many, char, lower, upper, string, sepBy, eof )
import Text.Parsec.String ( Parser )

import Helpers ( (-->) )
import Parsers.LowLevel ( ValueName, ValueType, value_name_p, value_type_p )

{-
  All:
  TypeName, FieldAndType, TupleValue, TupleType
-}

-- TypeName

newtype TypeName = TN String

instance Show TypeName where show = \(TN n) -> "TypeName " ++ n

type_name_p =
  upper >>= \u ->
  many (lower <|> upper) >>= \lu ->
  return $ TN (u:lu)
  :: Parser TypeName

-- FieldAndType

data FieldAndType = FieldAndType_ ValueName ValueType

instance Show FieldAndType where
  show = \(FieldAndType_ vn vt) -> show vn ++ " Type " ++ show vt

field_and_type_p = 
  value_name_p >>= \vn ->
  string ": " >> value_type_p >>= \vt ->
  return $ FieldAndType_ vn vt
  :: Parser FieldAndType
  
-- TupleValue

data TupleValue = FieldAndTypeList [ FieldAndType ]
  deriving ( Show )

tuple_value_p = 
  string "( " >> (field_and_type_p-->sepBy $ string ", ") >>= \fatl ->
  string " )" >> fatl-->FieldAndTypeList-->return
  :: Parser TupleValue

-- TupleType

data TupleType = NameAndTuple TypeName TupleValue
  deriving ( Show )

tuple_type_p =
  string "tuple_type " >> type_name_p  >>= \tn ->
  string "\nvalue " >> tuple_value_p  >>= \tv ->
  (char '\n' *> return () <|> eof) >> NameAndTuple tn tv-->return
  :: Parser TupleType

-- Helpers

parse_with = flip parse "input"
  :: Parser a -> String -> Either ParseError a


