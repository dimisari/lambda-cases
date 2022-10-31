{-# LANGUAGE LambdaCase #-}

module Parsers.LowLevel where

import Prelude
  ( String, Bool( True ), Eq, Show, (++), (<$>), (<*), (<*>), (*>), ($), (>>=), (>>)
  , return, show, elem, pure, concat, map )
import Text.Parsec ( (<|>), char, string, parserFail, many, many1, lower, try )
import Text.Parsec.String ( Parser )

import Helpers ( (-->), (.>), comma_seperated2, paren_comma_seperated2 )

{-
  All:
  Keywords, Literal, ValueName, AtomicExpression, ApplicationDirection,
  TupleMatchingExpression, AbstractionArgumentExpression, AbstractionArgumentsExpression,
  HighPrecedenceTypeExpression, TypeExpression
-}

-- Keywords

keywords =
  [ "tuple_type", "value", "or_type", "values"
  , "use_tuple_fields", "cases", "case_value", "intermediates", "output" 
  , "type_predicate", "function", "functions", "type_theorem", "proof" ]
  :: [ String ]

-- Literal

data Literal = Constant0 | Constant1
  deriving ( Eq, Show )

literal_p =
  char '0' *> return Constant0 <|> char '1' *> return Constant1
  :: Parser Literal

-- ValueName

newtype ValueName = Name String deriving ( Eq )
instance Show ValueName where show = \(Name n) -> "Name " ++ n

value_name_p =
  many1 (lower <|> char '_') >>= \l_ -> elem l_ keywords --> \case
    True -> parserFail "keyword"
    _ -> return $ Name l_
  :: Parser ValueName 

-- AtomicExpression

data AtomicExpression = ConstantExp Literal | NameExp ValueName
  deriving ( Eq )
instance Show AtomicExpression where
  show = \case
    ConstantExp e -> show e
    NameExp e -> show e

atomic_expression_p =
  ConstantExp <$> literal_p <|> NameExp <$> value_name_p
  :: Parser AtomicExpression

-- ApplicationDirection

data ApplicationDirection = LeftApplication | RightApplication 
  deriving ( Eq, Show )

application_direction_p = 
  string "<--" *> return LeftApplication <|> string "-->" *> return RightApplication
  :: Parser ApplicationDirection

-- TupleMatchingExpression

newtype TupleMatchingExpression = TupleMatching [ ValueName ]
  deriving ( Eq, Show )

tuple_matching_expression_p =
  TupleMatching <$> paren_comma_seperated2 value_name_p
  :: Parser TupleMatchingExpression

-- AbstractionArgumentExpression

data AbstractionArgumentExpression =
  NameExp_ab ValueName | TupleMatchingExp TupleMatchingExpression
  deriving ( Eq )

instance Show AbstractionArgumentExpression where
  show = \case
    NameExp_ab e -> show e
    TupleMatchingExp e -> show e

abstraction_argument_expression_p =
  NameExp_ab <$> value_name_p <|> TupleMatchingExp <$> tuple_matching_expression_p
  :: Parser AbstractionArgumentExpression

-- AbstractionArgumentsExpression

newtype AbstractionArgumentsExpression =
  AbstractionArguments [ AbstractionArgumentExpression ]
  deriving ( Eq )

instance Show AbstractionArgumentsExpression where
  show = \(AbstractionArguments aaes) ->
    aaes-->map (show .> (++ " abstraction "))-->concat

abstraction_arguments_expression_p =
  AbstractionArguments <$>
    many (try $ abstraction_argument_expression_p <* string " -> ")
  :: Parser AbstractionArgumentsExpression

-- HighPrecedenceTypeExpression

data HighPrecedenceTypeExpression =
  TupleType [ TypeExpression ] | ForPrecedenceType TypeExpression | IntType
  deriving ( Eq, Show )

high_precedence_type_expression_p =
  TupleType <$> try (paren_comma_seperated2 type_expression_p) <|>
  ForPrecedenceType <$> (char '(' *> type_expression_p <* char ')') <|>
  string "Int" *> pure IntType
  :: Parser HighPrecedenceTypeExpression

-- TypeExpression

data TypeExpression = Type [ HighPrecedenceTypeExpression ] HighPrecedenceTypeExpression
  deriving ( Eq )

instance Show TypeExpression where
  show = \(Type hptes hpte) ->
    hptes-->map (show .> (++ " right_arrow "))-->concat-->( ++ show hpte)

type_expression_p = try type_expression2_p <|> type_expression1_p
  :: Parser TypeExpression

type_expression1_p =
  many (try $ high_precedence_type_expression_p <* string " -> ") >>= \hptes ->
  high_precedence_type_expression_p >>= \hpte ->
  return $ Type hptes hpte
  :: Parser TypeExpression

type_expression2_p =
  comma_seperated2 type_expression1_p >>= \tes ->
  string " :> " >> type_expression1_p >>= \(Type hptes hpte) ->
  return $ Type (map ForPrecedenceType tes ++ hptes) hpte
  :: Parser TypeExpression
