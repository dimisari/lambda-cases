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
import qualified Data.Map as M
  ( empty, fromList )

import Helpers
  ( Haskell, (.>) )

import HaskellTypes.LowLevel
  ( ValueName(VN) )
import HaskellTypes.Types
  ( TypeName(TN), BaseType(..), ValueType(AbsTypesAndResType), TupleType )
import HaskellTypes.Values
  ( NamesTypesAndValues )
import HaskellTypes.Generation
  ( ValueMap, GenState(..), init_state )

import Parsers.Types
  ( tuple_type_p )
import Parsers.Values
  ( names_types_and_values_p )

import CodeGenerators.Types
  ( tuple_type_g )
import CodeGenerators.Values
  ( names_types_and_values_g )

-- Constants
[ example_name, io_files, haskell_header, example_lc, example_hs ] =
  [ "example", "IOfiles/"
  , io_files ++ "haskell_code_header.hs"
  , io_files ++ example_name ++ ".lc"
  , io_files ++ example_name ++ ".hs" ] 
  :: [ String ]

-- Parsing 
parse_with =
  flip parse $ example_name ++ ".lc"
  :: Parser a -> String -> Either ParseError a

data NTAVsOrTupleType =
  TupleType_ TupleType | NTAVs NamesTypesAndValues deriving Show

ntavs_or_tt_p =
  TupleType_ <$> try tuple_type_p <|> NTAVs <$> names_types_and_values_p
  :: Parser NTAVsOrTupleType

type Program = [ NTAVsOrTupleType ]

program_p =
  many (char '\n') *> many ntavs_or_tt_p <* eof 
  :: Parser Program

parse_string = parse_with program_p
  :: String -> Either ParseError Program

-- Generating haskell
program_g = mapM ( \case 
  NTAVs ntavs -> names_types_and_values_g ntavs
  TupleType_ tt -> tuple_type_g tt
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

main =
  readFile example_lc
  -- >>= parse_string .> print
  >>= write_code
  :: IO ()
