module Main where

import Text.Parsec (ParseError, (<|>), eof, many, parse, char, try)
import Text.Parsec.String (Parser)
import Control.Monad ((>=>))
import Control.Monad.State (evalState)
import Control.Monad.Trans.Except (runExceptT)

import GenerationState.InitialState (init_state)
import Helpers (Haskell, Error, (.>), (==>))

import GenerationState.TypesAndOperations (Stateful)

import ParsingTypes.TypeDefinitions (TypeDefinition)
import ParsingTypes.Values (NamesTypesAndValues)

import Parsers.TypeDefinitions (type_definition_p)
import Parsers.Values (names_types_and_values_p)

import CodeGenerators.TypeDefinitions (type_definition_g)
import CodeGenerators.Values (names_types_and_values_g)

-- All: Path, Constants, Types, Parsing, Generating Haskell, main

-- Path: Path
 
type Path = String 

-- Constants:
-- lcases_examples_names, lcases_examples_paths, examples_generated_haskell_paths,
-- examples_input_output_paths, haskell_header

lcases_examples_names =
  [ "my_gcd"
  , "ext_euc_no_tuple_type"
  , "ext_euc_tuple_type"
  , "basic_or_types"
  ]
  :: [ String ]

lcases_examples_paths =
  map ( \ex_name -> "lcases/" ++ ex_name ++ ".lc" ) lcases_examples_names
  :: [ Path ]

examples_generated_haskell_paths =
  map ( \ex_name -> "generated_haskell/" ++ ex_name ++ ".hs" ) lcases_examples_names
  :: [ Path ]

examples_input_output_paths = 
  zip lcases_examples_paths examples_generated_haskell_paths
  :: [ (Path, Path) ]

haskell_header =
  "haskell_headers/haskell_code_header.hs"
  :: Path

-- Types: NTAVsOrTypeDef, Program

data NTAVsOrTypeDef =
  TypeDefinition TypeDefinition | NTAVs NamesTypesAndValues deriving Show

type Program = [ NTAVsOrTypeDef ]

-- Parsing: parse_lcases, parse_with, program_p, ntavs_or_type_def_p

parse_lcases =
  parse program_p
  :: Path -> String -> Either ParseError Program

program_p =
  many (char '\n') *> many ntavs_or_type_def_p <* eof 
  :: Parser Program

ntavs_or_type_def_p =
  TypeDefinition <$> try type_definition_p <|> NTAVs <$> names_types_and_values_p
  :: Parser NTAVsOrTypeDef

-- Generating Haskell:
-- print_error_or_haskell_to_file, print_parse_error_or_semantic_analysis,
-- print_semantic_error_or_haskell_to_file, run_semantic_analysis, program_g
-- ntavs_or_type_def_g

read_and_generate_example = ( \(input_path, output_path) ->
  readFile input_path >>= \lcases_input ->
  parse_lcases input_path lcases_input ==> \parser_output ->
  print_parse_error_or_semantic_analysis parser_output output_path
  ) :: (Path, Path) -> IO ()

print_parse_error_or_semantic_analysis = ( \parser_output output_path ->
  case parser_output of 
    Left parse_error -> print parse_error
    Right program -> print_semantic_error_or_haskell_to_file program output_path
  ) :: Either ParseError Program -> Path -> IO ()

print_semantic_error_or_haskell_to_file = ( \program output_path ->
  run_semantic_analysis program ==> \case
    Left semantic_error -> putStrLn semantic_error
    Right generated_haskell -> 
      readFile haskell_header >>= \header ->
      writeFile output_path $ header ++ generated_haskell
  ) :: Program -> Path -> IO ()

run_semantic_analysis =
  program_g .> runExceptT .> flip evalState init_state
  :: Program -> Either Error Haskell

program_g =
  mapM ntavs_or_type_def_g .> fmap concat
  :: Program -> Stateful Haskell

ntavs_or_type_def_g = ( \case 
  NTAVs ntavs -> names_types_and_values_g ntavs
  TypeDefinition type_def -> type_definition_g type_def
  ) :: NTAVsOrTypeDef -> Stateful Haskell

-- main

main =
  mapM_ read_and_generate_example examples_input_output_paths
  :: IO ()
