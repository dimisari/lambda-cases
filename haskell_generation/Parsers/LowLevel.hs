{-# language LambdaCase #-}

module Parsers.LowLevel where

import Prelude ( Bool(..), (<$>), (<*), (*>), ($), (>>=), return, elem )
import Text.Parsec ( (<|>), char, string, parserFail, many, many1, lower, try )
import Text.Parsec.String ( Parser )

import Helpers ( (-->), keywords, paren_comma_seperated2 )
import HaskellTypes.LowLevel
  ( Literal(..), ValueName(..), LiteralOrValueName(..), ApplicationDirection(..)
  , TupleMatching(..), Abstraction(..), Abstractions(..) )

literal_p =
  char '0' *> return Constant0 <|> char '1' *> return Constant1
  :: Parser Literal

value_name_p =
  many1 (lower <|> char '_') >>= \l_ -> elem l_ keywords --> \case
    True -> parserFail "keyword"
    _ -> return $ VN l_
  :: Parser ValueName 

literal_or_value_name_p =
  Literal <$> literal_p <|> ValueName <$> value_name_p
  :: Parser LiteralOrValueName

application_direction_p = 
  string "<--" *> return LeftApplication <|> string "-->" *> return RightApplication
  :: Parser ApplicationDirection

tuple_matching_p =
  FieldNames <$> paren_comma_seperated2 value_name_p
  :: Parser TupleMatching

abstraction_p =
  ValueNameAb <$> value_name_p <|> TupleMatching <$> tuple_matching_p
  :: Parser Abstraction

abstractions_p =
  As <$> many (try $ abstraction_p <* string " -> ")
  :: Parser Abstractions
