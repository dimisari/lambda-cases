{-# LANGUAGE LambdaCase #-}

module Parsers.LowLevel where

import Prelude
  ( String, Bool( True ), Eq, Show, (++), (<$>), (<*), (<*>), (*>), ($), (>>=), return
  , show, elem, pure, concat, map )
import Text.Parsec ( (<|>), char, string, parserFail, many, many1, lower, try )
import Text.Parsec.String ( Parser )

import Helpers ( (-->), (.>), paren_comma_seperated2 )

{-
All:
Keywords, Literal, NameExpression, AtomicExpression, TupleMatchingExpression,
ApplicationDirection, IntOrIntTupleExpression, TypeExpression
-}

-- Keywords

keywords =
  [ "tuple_type", "value", "cases_type", "values"
  , "use_tuple_fields", "cases", "case_value", "intermediates", "output" 
  , "type_predicate", "function", "functions", "type_theorem", "proof" ]
  :: [ String ]

-- Literal

data Literal = Constant0 | Constant1 deriving ( Eq, Show )

constant_p =
  char '0' *> return Constant0 <|> char '1' *> return Constant1
  :: Parser Literal

-- NameExpression

newtype NameExpression = Name String deriving ( Eq )
instance Show NameExpression where show = \(Name n) -> "Name " ++ n

name_expression_p =
  many1 (lower <|> char '_') >>= \l_ -> elem l_ keywords --> \case
    True -> parserFail "keyword"
    _ -> return $ Name l_
  :: Parser NameExpression 

-- AtomicExpression

data AtomicExpression = ConstantExp Literal | NameExp NameExpression
  deriving ( Eq )
instance Show AtomicExpression where
  show = \case
    ConstantExp e -> show e
    NameExp e -> show e

atomic_expression_p =
  ConstantExp <$> constant_p <|> NameExp <$> name_expression_p
  :: Parser AtomicExpression

-- TupleMatchingExpression

newtype TupleMatchingExpression = TupleMatching [ NameExpression ] deriving ( Eq, Show )

tuple_matching_expression_p =
  TupleMatching <$> paren_comma_seperated2 name_expression_p
  :: Parser TupleMatchingExpression

-- ApplicationDirection

data ApplicationDirection = LeftApplication | RightApplication 
  deriving ( Eq, Show )

application_direction_p = 
  string "<--" *> return LeftApplication <|> string "-->" *> return RightApplication
  :: Parser ApplicationDirection

-- IntOrIntTupleExpression

data IntOrIntTupleExpression = TupleType [ TypeExpression ] | IntType
  deriving ( Eq, Show )

tuple_or_int_p =
  TupleType <$> paren_comma_seperated2 type_expression_p <|>
  string "Int" *> pure IntType
  :: Parser IntOrIntTupleExpression

-- TypeExpression

data TypeExpression = Type [ IntOrIntTupleExpression ] IntOrIntTupleExpression
  deriving ( Eq )

instance Show TypeExpression where
  show = \(Type ioites ioite) ->
    ioites-->map (show .> (++ " right_arrow "))-->concat-->( ++ show ioite)

type_expression_p =
  many (try $ tuple_or_int_p <* string " -> ") >>= \ioites ->
  tuple_or_int_p >>= \ioite ->
  return $ Type ioites ioite
  :: Parser TypeExpression
