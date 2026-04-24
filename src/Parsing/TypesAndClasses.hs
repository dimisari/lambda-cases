module Parsing.TypesAndClasses where

import Prelude qualified as P
import Text.Parsec qualified as TP

type Parser = TP.Parsec P.String ParserState

type ParserState = (IndentationLevel, InEqualLine)

type IndentationLevel = P.Int

type InEqualLine = P.Bool

class HasParser a where
  parser :: Parser a

class HasParserWithPrefix a b where
  parser_wp :: a -> Parser b
