{-# language LambdaCase #-}

module Main where

import Control.Monad
  ( (>=>) )
import Text.Parsec
  ( ParseError, (<|>), eof, many, parse, char, try )
import Text.Parsec.String
  ( Parser )
import Control.Monad.State
  ( evalState )

import InitialState
  ( init_state )
import Helpers
  ( Haskell, (.>) )

import HaskellTypes.Types
  ( TypeDef )
import HaskellTypes.Values
  ( NamesTypesAndValues )
import HaskellTypes.AfterParsing

import Parsers.Types
  ( type_def_p )
import Parsers.Values
  ( names_types_and_values_p )

import CodeGenerators.Types
  ( type_g )
import CodeGenerators.Values
  ( names_types_and_values_g )

-- All: Constants, Types, Parsing, Generating Haskell, main

-- Constants
[ example_name, files, haskell_header,
  example_lc, example_hs ] =
  [ files ++ "example", "files/", files ++ "haskell_code_header.hs"
  , example_name ++ ".lc", example_name ++ ".hs" ] 
  :: [ String ]

-- Types
data NTAVsOrType =
  TypeDef TypeDef | NTAVs NamesTypesAndValues deriving Show

type Program = [ NTAVsOrType ]

-- Parsing 
parse_with = flip parse example_lc
  :: Parser a -> String -> Either ParseError a

ntavs_or_tt_p =
  TypeDef <$> try type_def_p <|> NTAVs <$> names_types_and_values_p
  :: Parser NTAVsOrType

program_p =
  many (char '\n') *> many ntavs_or_tt_p <* eof 
  :: Parser Program

parse_string = parse_with program_p
  :: String -> Either ParseError Program

-- Generating Haskell
program_g = mapM ( \case 
  NTAVs ntavs -> names_types_and_values_g ntavs
  TypeDef t -> type_g t
  ) .> fmap concat .> flip evalState init_state
  :: Program -> Haskell

generate_code =
  parse_string >=> program_g .> return
  :: String -> Either ParseError Haskell

write_code = ( generate_code .> \case
  Left e -> print e
  Right source -> 
    readFile haskell_header >>= \header ->
    writeFile example_hs $ header ++ source
  ) :: String -> IO ()

-- main
main =
  readFile example_lc >>=
  write_code
  -- parse_string .> print
  :: IO ()
