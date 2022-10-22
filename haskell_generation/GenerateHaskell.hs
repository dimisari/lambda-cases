{-# LANGUAGE LambdaCase #-}

module GenerateHaskell where

import Prelude ( String, (++), undefined, map, concat )

import Parsers.LowLevel.Expressions
  ( TypeExpression(Type), TupleOrIntType(TupleType, IntType) )
import qualified Parsers.LowLevel.Expressions as N ( NameExpression(Name) )
import Parsers.LowLevel.Helpers ( (-->), (.>) )
import Parsers.ValueExpressions
  ( ValueExpression(Value), NameTypeAndValueExpression(NameTypeAndValue)
  , NameTypeAndValueExpressions(NameTypeAndValueExps)
  , AbstractionArgumentExpression(Name, TupleMatching))
import Parsers.ValueExpressions
import Main (main)

type HaskellSource = String

name_type_and_value_expressions_g = (\(NameTypeAndValueExps ntaves) ->
  map (name_type_and_value_expression_g .> (++ "\n")) ntaves-->concat
  ) :: NameTypeAndValueExpressions -> HaskellSource

name_type_and_value_expression_g = (\(NameTypeAndValue ne te ve) -> 
  name_expression_g ne ++ " = " ++
  "(" ++ value_expression_g ve ++ ")" ++
  " :: " ++ type_expression_g te ++ "\n"
  ) :: NameTypeAndValueExpression -> HaskellSource

name_expression_g = (\(N.Name n) -> n
  ) :: N.NameExpression -> HaskellSource

value_expression_g = (\(Value aaes nae) ->
  aaes-->map (abstraction_argument_expression_g .> (++ " -> "))-->concat-->
  (++ no_abstraction_expression_g nae)
  ) :: ValueExpression -> HaskellSource

type_expression_g = (\(Type toits toit) -> 
  toits-->map (tuple_or_int_g .> (++ " -> "))-->concat-->(++ tuple_or_int_g toit)
  ) :: TypeExpression -> HaskellSource

abstraction_argument_expression_g = ( undefined 
  ) :: AbstractionArgumentExpression -> HaskellSource

no_abstraction_expression_g = ( undefined 
  ) :: NoAbstractionsValueExpression -> HaskellSource

tuple_or_int_g = ( undefined 
  ) :: TupleOrIntType -> HaskellSource
