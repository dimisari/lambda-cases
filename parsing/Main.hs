module Main where

import Prelude
  ( undefined, String, Either, (<*), (*>), print, (>>=), readFile, IO )
import LowLevel.Helpers ( (.>), parse_with )
import Text.Parsec ( ParseError, eof, many, many1, parse, char )
import Text.Parsec.String ( Parser )
import ValueExpressions


filename = "/home/gnostis/Desktop/temp/temp.txt"
  :: String

program_p = many (char '\n') *> many1 name_type_and_value_p <* eof 
   :: Parser [ NameTypeAndValueExpression ]

main = readFile filename >>= parse_with program_p .> print
  :: IO ()
