{-# LANGUAGE LambdaCase #-}

module Main where

import Prelude
  ( IO, String, Either(..), Show, ($), (<$>), (++), (<*), (*>), (>>=), print, writeFile
  , readFile, flip, return, mapM, fmap, concat )
import Control.Monad ( (>=>) )
import Text.Parsec ( ParseError, (<|>), eof, many, parse, char )
import Text.Parsec.String ( Parser )
import Control.Monad.State ( evalState )

import Helpers ( Haskell, (.>) )

import HaskellTypes.Types ( TupleType )
import Parsers.Types ( tuple_type_p )
import CodeGenerators.Types ( tuple_type_g )

import HaskellTypes.Values ( NamesTypesAndValues )
import Parsers.Values ( names_types_and_values_p )
import CodeGenerators.Values ( names_types_and_values_g )

-- Constants

[ example_name, io_files, haskell_header, example_lc, example_hs ] =
  [ "example", "IOfiles/"
  , io_files ++ "haskell_code_header.hs"
  , io_files ++ example_name ++ ".lc"
  , io_files ++ example_name ++ ".hs" ] 
  :: [ String ]

-- Parsing 

parse_with = flip parse $ example_name ++ ".lc"
  :: Parser a -> String -> Either ParseError a

data NTAVsOrTupleType = NTAVs NamesTypesAndValues | TupleType TupleType
  deriving Show

ntavs_or_tt_p = NTAVs <$> names_types_and_values_p <|> TupleType <$> tuple_type_p
  :: Parser NTAVsOrTupleType

type Program = [ NTAVsOrTupleType ]

program_p = many (char '\n') *> many ntavs_or_tt_p <* eof 
  :: Parser Program

parse_string = parse_with program_p
  :: String -> Either ParseError Program

-- Generating haskell

program_g = mapM ( \case 
  NTAVs ntavs -> names_types_and_values_g ntavs
  TupleType tt -> return $ tuple_type_g tt
  ) .> fmap concat .> flip evalState 0 :: Program -> Haskell

generate_code = parse_string >=> program_g .> return
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
