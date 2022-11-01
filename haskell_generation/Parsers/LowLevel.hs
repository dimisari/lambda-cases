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
  Keywords, Literal, ValueName, LiteralOrValueName, ApplicationDirection,
  TupleMatching, Abstraction, Abstractions,
  TupleParenOrIntType, ValueType
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

literal_p = char '0' *> return Constant0 <|> char '1' *> return Constant1
  :: Parser Literal

-- ValueName

newtype ValueName = VN String
  deriving ( Eq )
instance Show ValueName where show = \(VN n) -> "Name " ++ n

value_name_p =
  many1 (lower <|> char '_') >>= \l_ -> elem l_ keywords --> \case
    True -> parserFail "keyword"
    _ -> return $ VN l_
  :: Parser ValueName 

-- LiteralOrValueName

data LiteralOrValueName = Literal Literal | ValueName ValueName
  deriving ( Eq )
instance Show LiteralOrValueName where
  show = \case
    Literal l -> show l
    ValueName vn -> show vn

literal_or_value_name_p = Literal <$> literal_p <|> ValueName <$> value_name_p
  :: Parser LiteralOrValueName

-- ApplicationDirection

data ApplicationDirection = LeftApplication | RightApplication 
  deriving ( Eq, Show )

application_direction_p = 
  string "<--" *> return LeftApplication <|> string "-->" *> return RightApplication
  :: Parser ApplicationDirection

-- TupleMatching

newtype TupleMatching = FieldNames [ ValueName ]
  deriving ( Eq, Show )

tuple_matching_p = FieldNames <$> paren_comma_seperated2 value_name_p
  :: Parser TupleMatching

-- Abstraction

data Abstraction = ValueNameAb ValueName | TupleMatching TupleMatching
  deriving ( Eq )

instance Show Abstraction where
  show = \case
    ValueNameAb vn -> show vn
    TupleMatching tm -> show tm

abstraction_p = ValueNameAb <$> value_name_p <|> TupleMatching <$> tuple_matching_p
  :: Parser Abstraction

-- Abstractions

newtype Abstractions = Abstractions [ Abstraction ]
  deriving ( Eq )

instance Show Abstractions where
  show = \(Abstractions as) -> as-->map (show .> (++ " abstraction "))-->concat

abstractions_p = Abstractions <$> many (try $ abstraction_p <* string " -> ")
  :: Parser Abstractions

-- TupleParenOrIntType

data TupleParenOrIntType = TupleType [ ValueType ] | ParenthesisType ValueType | IntType
  deriving ( Eq, Show )

tuple_paren_or_int_type_p =
  TupleType <$> try (paren_comma_seperated2 value_type_p) <|>
  ParenthesisType <$> (char '(' *> value_type_p <* char ')') <|>
  string "Int" *> pure IntType
  :: Parser TupleParenOrIntType

-- ValueType

data ValueType =
  AbstractionTypesAndResultType [ TupleParenOrIntType ] TupleParenOrIntType
  deriving ( Eq )

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
