module Main where

import Prelude
  ( undefined, String, Either, (<*), (*>), print, (>>=), readFile, IO, flip )
import LowLevel.Helpers ( (.>) )
import Text.Parsec ( ParseError, eof, many, many1, parse, char )
import Text.Parsec.String ( Parser )
import ValueExpressions

parse_with = flip parse "" 
  :: Parser a -> String -> Either ParseError a

filename = "/home/gnostis/Desktop/temp/temp.txt"
  :: String

program_p = many (char '\n') *> name_type_and_value_expressions_p <* eof 
   :: Parser NameTypeAndValueExpressions

main = readFile filename >>= parse_with program_p .> print
  :: IO ()
