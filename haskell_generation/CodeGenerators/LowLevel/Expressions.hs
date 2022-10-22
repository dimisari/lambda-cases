{-# LANGUAGE LambdaCase #-}

module CodeGenerators.LowLevel.Expressions where

import Prelude
  ( String, (++), concat, map, init, last, error )

import Parsers.LowLevel.Expressions
  ( Literal( Constant0, Constant1 ) 
  , NameExpression( Name )
  , TupleMatchingExpression( TupleMatching ) 
  , TypeExpression( Type )
  , TupleOrIntType( TupleType, IntType ) )
import Parsers.LowLevel.Helpers ( (-->), (.>) )

type HaskellSource = String

{-
All:
Literal, NameExpression, TupleMatchingExpression, AtomicExpression,
ApplicationDirection, TypeExpression, TupleOrIntType
-}

-- Literal

literal_g = ( \case
  Constant0 -> "0"
  Constant1 -> "1"
  ) :: Literal -> HaskellSource

-- NameExpression

name_expression_g = ( \(Name n) -> n
  ) :: NameExpression -> HaskellSource

-- TupleMatchingExpression

tuple_matching_expression_g = ( \(TupleMatching nes) -> nes --> \case
  [] -> error "should not have less than 2 in tuple"
  [ _ ] -> tuple_matching_expression_g (TupleMatching [])
  _ ->
    let
    all_but_last = init nes-->map (name_expression_g .> (++ ", "))-->concat
      :: String
    last_one = nes-->last-->name_expression_g
      :: String
    in
    "( " ++ all_but_last ++ last_one ++ " )"
  ) :: TupleMatchingExpression -> HaskellSource

-- AtomicExpression

-- data AtomicExpression = ConstantExp Literal | NameExp NameExpression
--   deriving (Eq)
-- instance Show AtomicExpression where
--   show = \case
--     ConstantExp e -> show e
--     NameExp e -> show e
-- 
-- atomic_expression_p = (ConstantExp <$> constant_p) <|> (NameExp <$> name_expression_p)
--   :: Parser AtomicExpression
-- 
-- ApplicationDirection

-- data ApplicationDirection = LeftApplication | RightApplication 
--   deriving ( Eq, Show )
-- 
-- application_direction_p = 
--   string "<--" *> return LeftApplication <|> string "-->" *> return RightApplication
--   :: Parser ApplicationDirection
-- 
-- TypeExpression

type_expression_g = ( \(Type toits toit) -> 
  toits-->map (tuple_or_int_g .> (++ " -> "))-->concat-->(++ tuple_or_int_g toit)
  ) :: TypeExpression -> HaskellSource

-- TupleOrIntType

tuple_or_int_g = ( \case
  TupleType tes -> "(" ++ (tes-->map (type_expression_g .> (++ ", "))-->concat) ++ ")"
  IntType -> "Int"
  ) :: TupleOrIntType -> HaskellSource

