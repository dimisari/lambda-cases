module Main where

import System.Process (callCommand)
import Text.Parsec (ParseError, (<|>), eof, many, parse, char, try)
import Text.Parsec.String (Parser)
import Control.Monad ((>=>))
import Control.Monad.State (evalState)
import Control.Monad.Trans.Except (runExceptT, catchE, throwE)

import Generation.State.InitialState (init_state)
import Helpers (Haskell, (.>), (==>), eof_or_spicy_nls)

import Generation.State.TypesAndOperations (Stateful, value_map_insert)

import Parsing.Types.TypeDefinitions (TypeDefinition)
import Parsing.Types.LowLevel (ValueName)
import Parsing.Types.Types (ValueType)
import Parsing.Types.Values (Values, ValueExpression)

import Parsing.Parsers.TypeDefinitions (type_definition_p)
import Parsing.Parsers.Values (values_p)

import Generation.Helpers.ErrorMessages (Error)

import Generation.Final.TypeDefinitions (type_definition_g)
import Generation.Final.Values (values_g, values_to_list, insert_value_to_map)

import Script

-- Types: ValuesOrTypeDef, Program

data ValuesOrTypeDef =
  TypeDefinition TypeDefinition | Values Values deriving Show

newtype Program =
  ValsOrTypeDefsList [ ValuesOrTypeDef ]

-- Parsing: program_p, values_or_type_def_p

program_p =
  many (char '\n') *> (ValsOrTypeDefsList <$> many values_or_type_def_p) <* eof
  :: Parser Program

values_or_type_def_p =
  (TypeDefinition <$> type_definition_p <|> Values <$> values_p)
  <* eof_or_spicy_nls
  :: Parser ValuesOrTypeDef

-- Generating Haskell:
-- read_and_gen_example, parse_err_or_sem_analysis,
-- sem_err_or_hs_to_file, run_sem_analysis, program_g
-- values_or_type_definition_g

type Paths = (Path, Path)

read_and_gen_example = ( \paths@(input_path, output_path) ->
  readFile input_path >>= parse_err_or_sem_analysis paths
  ) :: Paths -> IO ()

parse_err_or_sem_analysis = ( \(input_path, output_path) lcs_prog ->
  parse program_p input_path lcs_prog ==> \case
    Left parse_error -> print parse_error
    Right program -> sem_err_or_hs_to_file program output_path
  ) :: Paths -> String -> IO ()

sem_err_or_hs_to_file = ( \program output_path ->
  run_sem_analysis program ==> \case
    Left (_, _, sem_err_msg) -> putStrLn sem_err_msg
    Right generated_haskell -> 
      readFile haskell_header >>= \header ->
      writeFile output_path $ header ++ generated_haskell
  ) :: Program -> Path -> IO ()

run_sem_analysis = 
  program_g .> runExceptT .> flip evalState init_state
  :: Program -> Either Error Haskell

program_g = ( \(ValsOrTypeDefsList vals_or_type_defs_list) ->
  mapM_ insert_value_to_map (vals_or_type_defs_to_list vals_or_type_defs_list) >>
  mapM values_or_type_definition_g vals_or_type_defs_list ==> fmap concat
  ) :: Program -> Stateful Haskell

vals_or_type_defs_to_list = ( \vals_or_type_defs_list ->
  concatMap vals_or_type_def_to_list vals_or_type_defs_list
  ) :: [ ValuesOrTypeDef ] -> [ (ValueName, ValueType, ValueExpression) ]

vals_or_type_def_to_list = ( \case 
  Values values -> values_to_list values
  TypeDefinition _ -> []
  ) :: ValuesOrTypeDef -> [ (ValueName, ValueType, ValueExpression) ]

values_or_type_definition_g = ( \case 
  Values values -> values_g values
  TypeDefinition type_definition -> type_definition_g type_definition
  ) :: ValuesOrTypeDef -> Stateful Haskell

-- main

main =
  mapM_ read_and_gen_example path_pairs >>
  mapM_ exec_path_pair_to_cmd exec_path_pairs >>
  callCommand
    ("rm " ++ hs_dir ++ "correct/*.hi " ++ hs_dir ++ "correct/*.o") >>
  callCommand 
    ("for f in " ++ execs_dir ++ "correct/*; do echo \"\n$f\n\"; $f; done")
  :: IO ()
