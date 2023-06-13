module Experimental.Indent where

import Text.Parsec
import Text.Parsec.Indent

import Helpers ((.>))

type Parser a = IndentParser String () a 

file = "src/Experimental/file.txt"

parseFile p = readFile file >>= runIndentParser p () file .> print

a =
  spaces >> withPos (char 'b' >> spaces >> checkIndent >>block (char 'a' <* spaces))
  :: Parser [ Char ]
