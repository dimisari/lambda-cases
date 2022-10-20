{-# LANGUAGE LambdaCase #-}

module LowLevel.AtomicExpression where

import Prelude
  ( String, Bool(True), Eq, Show, (++), (<$>), (<*), (*>), ($), (>>=), return, show
  , elem )
import Text.Parsec ( (<|>), char, string, parserFail )
import Text.Parsec.String ( Parser )
import LowLevel.Helpers ( lowers_underscore, seperated2, (-->) )

{-
All:
Keywords, ConstantExpression, NameExpression, TupleMatchingExpression, AtomicExpression
-}

-- Keywords

keywords =
  [ "tuple_type", "value", "case_type", "values"
  , "use_tuple_fields", "case", "case_value", "intermediates", "output" 
  , "type_predicate", "function", "functions", "type_theorem", "proof" ]
  :: [ String ]

-- ConstantExpression

data ConstantExpression = Constant0 | Constant1 deriving ( Eq, Show )

constant_p = char '0' *> return Constant0 <|> char '1' *> return Constant1
  :: Parser ConstantExpression

-- NameExpression

newtype NameExpression = Name String deriving ( Eq )
instance Show NameExpression where show = \(Name n) -> "Name " ++ n

name_expression_p =
  lowers_underscore >>= \lu -> elem lu keywords --> \case
    True -> parserFail "keyword"
    _ -> return $ Name lu
  :: Parser NameExpression 

-- TupleMatchingExpression

newtype TupleMatchingExpression = TupleMatching [ NameExpression ] deriving ( Eq, Show )

tuple_matching_expression_p =
  TupleMatching <$> (string "( " *> seperated2 name_expression_p ", " <* string " )")
  :: Parser TupleMatchingExpression

-- AtomicExpression

data AtomicExpression = ConstantExp ConstantExpression | NameExp NameExpression
  deriving (Eq)
instance Show AtomicExpression where
  show = \case
    ConstantExp e -> show e
    NameExp e -> show e

atomic_expression_p = (ConstantExp <$> constant_p) <|> (NameExp <$> name_expression_p)
  :: Parser AtomicExpression
