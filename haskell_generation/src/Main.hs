module Main where

import Control.Monad
  ( (>=>) )
import Text.Parsec
  ( ParseError, (<|>), eof, many, parse, char, try )
import Text.Parsec.String
  ( Parser )
import Control.Monad.State
  ( evalState )
import Control.Monad.Trans.Except
  ( runExceptT )

import InitialState
  ( init_state )
import Helpers
  ( Haskell, Error, (.>) )

import HaskellTypes.Generation
   ( Stateful )
import HaskellTypes.AfterParsing
  ( TypeDef, type_def_conversion )
import HaskellTypes.Values
  ( NamesTypesAndValues )

import Parsers.Types
  ( type_def_p )
import Parsers.Values
  ( names_types_and_values_p )

import CodeGenerators.Types
  ( val_type_def_g )
import CodeGenerators.Values
  ( names_types_and_values_g )

-- All: Constants, Types, Parsing, Generating Haskell, main

-- Constants

[ example_name, files, haskell_header, example_lc, example_hs ] =
  [ files ++ "example", "files/", files ++ "haskell_code_header.hs"
  , example_name ++ ".lc", example_name ++ ".hs" ] 
  :: [ String ]

-- Types

data NTAVsOrTypeDef =
  TypeDefinition TypeDef | NTAVs NamesTypesAndValues deriving Show

type Program = [ NTAVsOrTypeDef ]

-- Parsing 

parse_with = flip parse example_lc
  :: Parser a -> String -> Either ParseError a

ntavs_or_tt_p =
  TypeDefinition <$> type_def_conversion <$> try type_def_p <|>
  NTAVs <$> names_types_and_values_p
  :: Parser NTAVsOrTypeDef

program_p = many (char '\n') *> many ntavs_or_tt_p <* eof 
  :: Parser Program

parse_string = parse_with program_p
  :: String -> Either ParseError Program

-- Generating Haskell

ntavs_or_type_def_g = ( \case 
  NTAVs ntavs -> names_types_and_values_g ntavs
  TypeDefinition t -> val_type_def_g t
  ) :: NTAVsOrTypeDef -> Stateful Haskell

program_g = mapM ntavs_or_type_def_g .> fmap concat
  :: Program -> Stateful Haskell
 
program_to_haskell = program_g .> runExceptT .> flip evalState init_state
  :: Program -> Either Error Haskell

generate_haskell = ( \case 
  Left error -> Left $ show error 
  Right program -> program_to_haskell program
  ) :: Either ParseError Program -> Either Error Haskell

write_haskell = (
  parse_string .> generate_haskell .> \case
    Left e -> putStrLn e
    Right haskell -> 
      readFile haskell_header >>= \header ->
      writeFile example_hs $ header ++ haskell
  ) :: String -> IO ()

-- main

main = readFile example_lc >>= write_haskell
  :: IO ()
