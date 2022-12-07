{-# language LambdaCase #-}

module Parsers.LowLevel where

import Text.Parsec
  ( (<|>), char, string, parserFail, many, many1, lower, try )
import Text.Parsec.String
  ( Parser )

import Helpers
  ( (==>), keywords, seperated2, integer )

import HaskellTypes.LowLevel
  ( Literal(..), ValueName(..), LiteralOrValueName(..), ApplicationDirection(..)
  , TupleMatching(..), Abstraction(..), Abstractions(..) )

-- All:
-- literal_p, value_name_p, literal_or_value_name_p, application_direction_p,
-- tuple_matching_p, abstraction_p, abstractions_p
literal_p = integer
  :: Parser Literal

value_name_p =
  many1 (lower <|> char '_') >>= \l_ -> elem l_ keywords ==> \case
    True -> parserFail "keyword"
    _ -> return $ VN l_
  :: Parser ValueName 

literal_or_value_name_p =
  Literal <$> literal_p <|> ValueName <$> value_name_p
  :: Parser LiteralOrValueName

application_direction_p = 
  string "<==" *> return LeftApplication <|>
  string "==>" *> return RightApplication
  :: Parser ApplicationDirection

tuple_matching_p =
  fmap TM $ string "( " *> seperated2 ", " value_name_p <* string " )"
  :: Parser TupleMatching

abstraction_p =
  ValueNameAb <$> value_name_p <|> TupleMatching <$> tuple_matching_p
  :: Parser Abstraction

abstractions_p =
  As <$> many (try $ abstraction_p <* string " -> ")
  :: Parser Abstractions
