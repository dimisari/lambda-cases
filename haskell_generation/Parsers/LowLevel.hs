{-# LANGUAGE LambdaCase #-}

module Parsers.LowLevel where

import Prelude
  ( String, Bool( True ), Show, (++), (<$>), (<*), (<*>), (*>), ($), (>>=), (>>)
  , return, show, elem, pure, concat, map )
import Text.Parsec ( (<|>), char, string, parserFail, many, many1, lower, try )
import Text.Parsec.String ( Parser )

import Helpers ( (-->), (.>), keywords, comma_seperated2, paren_comma_seperated2 )

{-
  All:
  Literal, ValueName, LiteralOrValueName, ApplicationDirection,
  TupleMatching, Abstraction, Abstractions, TupleParenOrIntType, ValueType
-}

-- Literal

data Literal = Constant0 | Constant1
  deriving ( Show )

literal_p = char '0' *> return Constant0 <|> char '1' *> return Constant1
  :: Parser Literal

-- ValueName

newtype ValueName = VN String

instance Show ValueName where show = \(VN n) -> "Name " ++ n

value_name_p = many1 (lower <|> char '_') >>= \l_ -> elem l_ keywords --> \case
  True -> parserFail "keyword"
  _ -> return $ VN l_
  :: Parser ValueName 

-- LiteralOrValueName

data LiteralOrValueName = Literal Literal | ValueName ValueName

instance Show LiteralOrValueName where
  show = \case
    Literal l -> show l
    ValueName vn -> show vn

literal_or_value_name_p = Literal <$> literal_p <|> ValueName <$> value_name_p
  :: Parser LiteralOrValueName

-- ApplicationDirection

data ApplicationDirection = LeftApplication | RightApplication 
  deriving ( Show )

application_direction_p = 
  string "<--" *> return LeftApplication <|> string "-->" *> return RightApplication
  :: Parser ApplicationDirection

-- TupleMatching

newtype TupleMatching = FieldNames [ ValueName ]
  deriving ( Show )

tuple_matching_p = FieldNames <$> paren_comma_seperated2 value_name_p
  :: Parser TupleMatching

-- Abstraction

data Abstraction = ValueNameAb ValueName | TupleMatching TupleMatching

instance Show Abstraction where
  show = \case
    ValueNameAb vn -> show vn
    TupleMatching tm -> show tm

abstraction_p = ValueNameAb <$> value_name_p <|> TupleMatching <$> tuple_matching_p
  :: Parser Abstraction

-- Abstractions

newtype Abstractions = Abstractions [ Abstraction ]

instance Show Abstractions where
  show = \(Abstractions as) -> as-->map (show .> (++ " abstraction "))-->concat

abstractions_p = Abstractions <$> many (try $ abstraction_p <* string " -> ")
  :: Parser Abstractions

