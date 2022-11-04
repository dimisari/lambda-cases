{-# LANGUAGE LambdaCase #-}

module Parsers.Types where

import Prelude
  ( String, Either, Show, (<$>), (>>=), (>>), (<*), (*>), (++), ($), show, return, flip
  , map, concat, pure )
import Text.Parsec
  ( ParseError, (<|>), parse, many, char, lower, upper, string, sepBy, eof, skipMany1
  , try )
import Text.Parsec.String ( Parser )

import Helpers
  ( (-->), (.>), new_line_space_surrounded, comma_seperated2, paren_comma_seperated2 )
import Parsers.LowLevel ( ValueName, value_name_p )

{-
  All:
  BaseType, ValueType, TypeName, FieldAndType, TupleValue, TupleType
-}

-- BaseType

data BaseType =
  TupleType [ ValueType ] | ParenthesisType ValueType | TypeName TypeName | IntType

instance Show BaseType where
  show = \case 
    TupleType vts -> "TupleType " ++ show vts
    ParenthesisType vt -> case vt of
      (AbstractionTypesAndResultType [] IntType) -> show IntType
      _ -> show vt
    TypeName tn -> show tn
    IntType -> "Int"

tuple_paren_or_int_type_p =
  TupleType <$> try (paren_comma_seperated2 value_type_p) <|>
  ParenthesisType <$> (char '(' *> value_type_p <* char ')') <|>
  TypeName <$> type_name_p <|>
  string "Int" *> pure IntType
  :: Parser BaseType

-- ValueType

data ValueType = AbstractionTypesAndResultType [ BaseType ] BaseType

instance Show ValueType where
  show = \(AbstractionTypesAndResultType tpoits tpoit) ->
    tpoits-->map (show .> (++ " right_arrow "))-->concat ++ show tpoit

value_type_p = try value_type_2_p <|> value_type_1_p
  :: Parser ValueType

value_type_1_p =
  many (try $ tuple_paren_or_int_type_p <* string " -> ") >>= \tpoits ->
  tuple_paren_or_int_type_p >>= \tpoit ->
  return $ AbstractionTypesAndResultType tpoits tpoit
  :: Parser ValueType

value_type_2_p =
  comma_seperated2 value_type_1_p >>= \tes ->
  string " :> " >> value_type_1_p >>= \(AbstractionTypesAndResultType tpoits tpoit) ->
  return $ AbstractionTypesAndResultType (map ParenthesisType tes ++ tpoits) tpoit
  :: Parser ValueType

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

instance Show TupleType where
  show = \(NameAndTuple tn tv) -> "\nname:" ++ show tn ++ "\ntuple: " ++ show tv ++ "\n"

tuple_type_p =
  string "tuple_type " >> type_name_p >>= \tn ->
  string "\nvalue " >> tuple_value_p >>= \tv ->
  (eof <|> skipMany1 new_line_space_surrounded) >> NameAndTuple tn tv-->return
  :: Parser TupleType

-- Helpers

parse_with = flip parse "input"
  :: Parser a -> String -> Either ParseError a
