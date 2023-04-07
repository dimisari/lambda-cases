module Main where

import Text.Parsec (ParseError, (<|>), eof, many, parse, char, try)
import Text.Parsec.String (Parser)
import Control.Monad ((>=>))
import Control.Monad.State (evalState)
import Control.Monad.Trans.Except (runExceptT)

import GenerationState.InitialState (init_state)
import Helpers (Haskell, Error, (.>), (==>), eof_or_new_lines)

import GenerationState.TypesAndOperations (Stateful, value_map_insert)

import ParsingTypes.TypeDefinitions (TypeDefinition)
import ParsingTypes.LowLevel (ValueName)
import ParsingTypes.Types (ValueType)
import ParsingTypes.Values (Values, ValueExpression)

import Parsers.TypeDefinitions (type_definition_p)
import Parsers.Values (values_p)

import Conversions.Types (value_type_conversion)

import CodeGenerators.TypeDefinitions (type_definition_g)
import CodeGenerators.Values (values_g, values_to_list)

-- All: Path, Constants, Types, Parsing, Generating Haskell, main

-- Path: Path
 
type Path = String 

-- Constants:
-- lcases_examples_names, lcases_examples_paths, examples_generated_haskell_paths,
-- examples_input_output_paths, haskell_header

lcases_examples_names =
  [-- "my_gcd"
  --, "ext_euc_no_tuple_type"
  --, "ext_euc_tuple_type"
  --, "basic_or_types"
  --, "pair"
  --, "int_or_string"
  "ok"
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

-- Types: ValuesOrTypeDefinition, Program

data ValuesOrTypeDefinition =
  TypeDefinition TypeDefinition | Values Values deriving Show

newtype Program =
  ValsOrTypeDefsList [ ValuesOrTypeDefinition ]

-- Parsing: parse_lcases, parse_with, program_p, ntavs_or_type_def_p

parse_lcases =
  parse program_p
  :: Path -> String -> Either ParseError Program

program_p =
  many (char '\n') *> (ValsOrTypeDefsList <$> many ntavs_or_type_def_p) <* eof
  :: Parser Program

ntavs_or_type_def_p =
  (TypeDefinition <$> try type_definition_p <|>
  Values <$> values_p) <* eof_or_new_lines
  :: Parser ValuesOrTypeDefinition

-- Generating Haskell:
-- print_error_or_haskell_to_file, print_parse_error_or_semantic_analysis,
-- print_semantic_error_or_haskell_to_file, run_semantic_analysis, program_g
-- values_or_type_definition_g

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

program_g = ( \(ValsOrTypeDefsList vals_or_type_defs_list) ->
  mapM_ insert_value_to_map (vals_or_type_defs_to_list vals_or_type_defs_list) >>
  mapM values_or_type_definition_g vals_or_type_defs_list ==> fmap concat
  ) :: Program -> Stateful Haskell

insert_value_to_map = ( \(value_name, value_type, value_expr) ->
  value_map_insert value_name $ value_type_conversion value_type
  ) :: (ValueName, ValueType, ValueExpression) -> Stateful ()

vals_or_type_defs_to_list = ( \vals_or_type_defs_list ->
  concatMap vals_or_type_def_to_list vals_or_type_defs_list
  ) :: [ ValuesOrTypeDefinition ] -> [ (ValueName, ValueType, ValueExpression) ]

vals_or_type_def_to_list = ( \case 
  Values values -> values_to_list values
  TypeDefinition _ -> []
  ) :: ValuesOrTypeDefinition -> [ (ValueName, ValueType, ValueExpression) ]

values_or_type_definition_g = ( \case 
  Values values -> values_g values
  TypeDefinition type_definition -> type_definition_g type_definition
  ) :: ValuesOrTypeDefinition -> Stateful Haskell

-- main

main =
  mapM_ read_and_generate_example examples_input_output_paths
  :: IO ()
