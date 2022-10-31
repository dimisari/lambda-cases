{-# LANGUAGE LambdaCase #-}

module Main where

import Prelude
  ( IO, String, Either( Left, Right ), ($), (++), (<*), (*>), (>>=), print, writeFile
  , readFile, flip, return )
import Control.Monad ( (>=>) )
import Text.Parsec ( ParseError, eof, many, parse, char )
import Text.Parsec.String ( Parser )
import Control.Monad.State ( evalState )

import Helpers ( (.>) )
import Parsers.ValueExpressions
  ( NameTypeAndValueExpressions, name_type_and_value_expressions_p )

import CodeGenerators.ValueExpressions
  ( HaskellSource, name_type_and_value_expressions_g )

-- Constants

[ example_name, io_files, haskell_header, example_lc, example_hs ] =
  [ "example", "IOfiles/", io_files ++ "haskell_code_header.hs"
  , io_files ++ example_name ++ ".lc", io_files ++ example_name ++ ".hs" ] 
  :: [ String ]

-- Parsing 

parse_with = flip parse $ example_name ++ ".lc"
  :: Parser a -> String -> Either ParseError a

type Program = NameTypeAndValueExpressions

program_p = many (char '\n') *> name_type_and_value_expressions_p <* eof 
  :: Parser Program

parse_string = parse_with program_p
  :: String -> Either ParseError Program

-- Generating haskell

program_g = name_type_and_value_expressions_g .> flip evalState 0
  :: Program -> HaskellSource

generate_code = parse_string >=> program_g .> return
  :: String -> Either ParseError HaskellSource

write_code = ( generate_code .> \case
  Left e -> print e
  Right source -> 
    readFile haskell_header >>= \header ->
    writeFile example_hs $ header ++ source
  ) :: String -> IO ()

main = readFile example_lc >>= write_code
  -- >>= parse_string .> print
  :: IO ()
