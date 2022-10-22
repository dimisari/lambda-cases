{-# LANGUAGE LambdaCase #-}

module GenerateHaskell where

import Prelude ( String, (++), undefined, map, concat, reverse, init, last, error )

import Parsers.LowLevel.Helpers ( (-->), (.>) )
import Parsers.ValueExpressions
  ( ValueExpression(Value), NameTypeAndValueExpression(NameTypeAndValue)
  , NameTypeAndValueExpressions(NameTypeAndValueExps)
  , AbstractionArgumentExpression(Name, TupleMatching))
import Parsers.ValueExpressions

import CodeGenerators.LowLevel.Expressions
  ( tuple_matching_expression_g, name_expression_g, type_expression_g )

import Main (main)

type HaskellSource = String

name_type_and_value_expressions_g = ( \(NameTypeAndValueExps ntaves) ->
  map (name_type_and_value_expression_g .> (++ "\n")) ntaves-->concat
  ) :: NameTypeAndValueExpressions -> HaskellSource

name_type_and_value_expression_g = ( \(NameTypeAndValue ne te ve) -> 
  name_expression_g ne ++ " = " ++
  "( " ++ value_expression_g ve ++ "\n" ++
  ") :: " ++ type_expression_g te ++ "\n"
  ) :: NameTypeAndValueExpression -> HaskellSource

value_expression_g = ( \(Value aaes nae) ->
  aaes-->map (abstraction_argument_expression_g .> (++ " -> "))-->concat-->
  (++ no_abstraction_expression_g nae)
  ) :: ValueExpression -> HaskellSource

abstraction_argument_expression_g = ( \case
  Name n -> name_expression_g n
  TupleMatching tm -> tuple_matching_expression_g tm
  ) :: AbstractionArgumentExpression -> HaskellSource

no_abstraction_expression_g = ( undefined 
  ) :: NoAbstractionsValueExpression -> HaskellSource
