module Main where

import Prelude ( String, Either, (<*), (*>), (>>=), print, readFile, IO, flip )
import Text.Parsec ( ParseError, eof, many, parse, char )
import Text.Parsec.String ( Parser )

import Parsers.LowLevel.Helpers ( (.>) )
import Parsers.ValueExpressions
  ( NameTypeAndValueExpressions, name_type_and_value_expressions_p )

filename = "example.lc"
  :: String

parse_with = flip parse filename
  :: Parser a -> String -> Either ParseError a

program_p = many (char '\n') *> name_type_and_value_expressions_p <* eof 
  :: Parser NameTypeAndValueExpressions

main = readFile filename >>= parse_with program_p .> print
  :: IO ()
