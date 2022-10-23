{-# LANGUAGE LambdaCase #-}

module Main where

import Prelude
  ( String, Either( Left, Right ), ($), (++), (<*), (*>), (>>=), print, writeFile
  , readFile, IO, flip, return, error )
import Control.Monad ( (>=>) )
import Text.Parsec ( ParseError, eof, many, parse, char )
import Text.Parsec.String ( Parser )

import Helpers ( (.>) )
import Parsers.ValueExpressions
  ( NameTypeAndValueExpressions, name_type_and_value_expressions_p )

import CodeGenerators.LowLevel ()
import CodeGenerators.ValueExpressions (name_type_and_value_expressions_g)

filename = "example.lc"
  :: String

parse_with = flip parse filename
  :: Parser a -> String -> Either ParseError a

program_p = many (char '\n') *> name_type_and_value_expressions_p <* eof 
  :: Parser NameTypeAndValueExpressions

parse_string = parse_with program_p
  :: String -> Either ParseError NameTypeAndValueExpressions

type HaskellSource = String

generate_code = parse_string >=> name_type_and_value_expressions_g .> return
  :: String -> Either ParseError HaskellSource

lambda_case_extenstion = "{-# LANGUAGE LambdaCase #-}\n\n"
  :: HaskellSource

write_code = (generate_code .> \case
  Left e -> print e
  Right source -> writeFile "example.hs" $ lambda_case_extenstion ++ source
  ) :: String -> IO ()

main = readFile filename >>= write_code
  :: IO ()
