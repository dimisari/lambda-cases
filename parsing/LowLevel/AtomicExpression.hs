{-# LANGUAGE LambdaCase #-}

module LowLevel.AtomicExpression where

import Prelude
  ( Eq, show, Show, String, (<$>), pure, (<*), (*>), (++), Bool(True), ($), return
  , (==), elem )
import Text.Parsec ( (<|>), char, string, parserFail )
import Text.Parsec.String ( Parser )
import LowLevel.Helpers ( lowers_underscore, seperated2 )

-- ConstantExpression

data ConstantExpression = Constant0 | Constant1 deriving ( Eq )

instance Show ConstantExpression where
  show = \case
    Constant0 -> "0."
    Constant1 -> "1."

[ zero_p, one_p, constant_p ] =
  [ char '0' *> pure Constant0, char '1' *> pure Constant1, zero_p <|> one_p ]
  :: [ Parser ConstantExpression ]

-- NameExpression

newtype NameExpression = NameExpression String deriving ( Eq )

instance Show NameExpression where show  = \(NameExpression n) -> n ++ "."

name_expression_p = do
  lu <- lowers_underscore
  let keywords =
        [ "tuple_type", "value", "case_type", "values"
        , "match_tuple", "case", "case_value", "intermediates", "output" 
        , "type_predicate", "function", "functions", "type_theorem", "proof" ]
        :: [ String ]
  case elem lu keywords of 
    True -> parserFail "keyword"
    _ -> return $ NameExpression lu
  :: Parser NameExpression

-- TupleMatchingExpression

newtype TupleMatchingExpression = NameExpressions [ String ] deriving ( Eq, Show )

tuple_matching_expression_p =
  NameExpressions <$> (string "( " *> seperated2 lowers_underscore ", " <* string " )")
  :: Parser TupleMatchingExpression

-- AtomicExpression

data AtomicExpression = Constant ConstantExpression | Name NameExpression
  deriving (Eq, Show)

atomic_expression_p = (Constant <$> constant_p) <|> (Name <$> name_expression_p)
  :: Parser AtomicExpression
