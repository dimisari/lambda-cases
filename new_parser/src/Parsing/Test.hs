{-# LANGUAGE LambdaCase #-}

module Parsing.Test where

import Text.Parsec (runParser, eof, ParseError)

import System.Directory
import Data.List.Split

import ASTTypes
import Parsing.AST
import ShowInstances
import Helpers

-- types
type ParseResultString = String

type ParseFunc = TestExample -> ParseResultString

type ParseToResStr a = TestExample -> ResultString a
newtype ResultString a = RS ParseResultString

-- paths
res_dir = "../parsing_results/"
  :: FilePath

-- main
main :: IO ()
main =
  list_progs >>= mapM_ read_prog_parse_write_res >>
  mapM_ run_parse_func_for_test_exs_file test_exs_file_name_parse_func_pairs

-- read_prog_parse_write_res 
read_prog_parse_write_res :: ProgramFileName -> IO ()
read_prog_parse_write_res pfn =
  readFile in_path >>= parse_program .> writeFile out_path
  where
  (in_path, out_path) =
    (in_dir ++ progs_dir ++ pfn, res_dir ++ progs_dir ++ pfn)
    :: (FilePath, FilePath)

  parse_program :: ParseFunc
  parse_program =
    (parse_and_ret_res_str :: ParseToResStr Program) .> extract_res_str

-- run_parse_func_for_test_exs_file
run_parse_func_for_test_exs_file :: (FileName, ParseFunc) -> IO ()
run_parse_func_for_test_exs_file (file_name, parse_func) =
  readFile in_path >>= in_str_to_out_str .> writeFile out_path
  where
  (in_path, out_path) =
    (in_dir ++ test_exs_dir ++ file_name, res_dir ++ test_exs_dir ++ file_name)
    :: (FilePath, FilePath)

  in_str_to_out_str :: FileString -> FileString
  in_str_to_out_str = file_string_to_examples .> concatMap parse_func

  file_string_to_examples :: FileString -> [ TestExample ]
  file_string_to_examples = endBy "#\n\n"

-- parse, parse_and_ret_res_str
parse :: HasParser a => String -> Either ParseError a
parse = runParser (parser <* eof) (0, False) "" 

parse_and_ret_res_str :: (HasParser a, Show a) => ParseToResStr a
parse_and_ret_res_str =
  parse .> res_to_str
  where
  res_to_str :: Show a => Either ParseError a -> ResultString a
  res_to_str = RS <$> \case
    Left err -> "Error :( ==>" ++ show err ++ "\n\n"
    Right res -> "Parsed :) ==>\n" ++ show res ++ "\n\n"

-- test_exs_file_name_parse_func_pairs
extract_res_str :: ResultString a -> ParseResultString
extract_res_str = \(RS s) -> s

test_exs_file_name_parse_func_pairs :: [(FileName, ParseFunc)]
test_exs_file_name_parse_func_pairs =
  [ ( "literals.txt"
    , (parse_and_ret_res_str :: ParseToResStr Literal) .> extract_res_str
    )
  , ( "identifiers.txt"
    , (parse_and_ret_res_str :: ParseToResStr Identifier) .> extract_res_str
    )
  , ( "paren_expr.txt"
    , (parse_and_ret_res_str :: ParseToResStr ParenExpr) .> extract_res_str
    )
  , ( "tuple.txt"
    , (parse_and_ret_res_str :: ParseToResStr Tuple) .> extract_res_str
    )
  , ( "big_tuple.txt"
    , (parse_and_ret_res_str :: ParseToResStr BigTuple) .> extract_res_str
    )
  , ( "list.txt"
    , (parse_and_ret_res_str :: ParseToResStr List) .> extract_res_str
    )
  , ( "big_list.txt"
    , (parse_and_ret_res_str :: ParseToResStr BigList) .> extract_res_str
    )
  , ( "paren_func_app.txt"
    , (parse_and_ret_res_str :: ParseToResStr ParenFuncApp) .> extract_res_str
    )
  , ( "prefix_func_app.txt"
    , (parse_and_ret_res_str :: ParseToResStr PreFuncApp) .> extract_res_str
    )
  , ( "postfix_func_app.txt"
    , (parse_and_ret_res_str :: ParseToResStr PostFuncApp) .> extract_res_str
    )
  , ( "line_op_expr.txt"
    , (parse_and_ret_res_str :: ParseToResStr LineOpExpr) .> extract_res_str
    )
  , ( "big_op_expr.txt"
    , (parse_and_ret_res_str :: ParseToResStr BigOpExpr) .> extract_res_str
    )
  , ( "line_func_expr.txt"
    , (parse_and_ret_res_str :: ParseToResStr LineFuncExpr) .> extract_res_str
    )
  , ( "big_func_expr.txt"
    , (parse_and_ret_res_str :: ParseToResStr BigFuncExpr) .> extract_res_str
    )
  , ( "cases_func_expr.txt"
    , (parse_and_ret_res_str :: ParseToResStr CasesFuncExpr) .> extract_res_str
    )
  , ( "value_def.txt"
    , (parse_and_ret_res_str :: ParseToResStr ValueDef) .> extract_res_str
    )
  , ( "grouped_val_defs.txt"
    , (parse_and_ret_res_str :: ParseToResStr GroupedValueDefs) .>
      extract_res_str
    )
  , ( "where_expr.txt"
    , (parse_and_ret_res_str :: ParseToResStr WhereExpr) .> extract_res_str
    )
  , ( "type_id.txt"
    , (parse_and_ret_res_str :: ParseToResStr TypeId) .> extract_res_str
    )
  , ( "type_var.txt"
    , (parse_and_ret_res_str :: ParseToResStr TypeVar) .> extract_res_str
    )
  , ( "func_type.txt"
    , (parse_and_ret_res_str :: ParseToResStr FuncType) .> extract_res_str
    )
  , ( "prod_type.txt"
    , (parse_and_ret_res_str :: ParseToResStr ProdType) .> extract_res_str
    )
  , ( "type_app.txt"
    , (parse_and_ret_res_str :: ParseToResStr TypeApp) .> extract_res_str
    )
  , ( "cond_type.txt"
    , (parse_and_ret_res_str :: ParseToResStr Type) .> extract_res_str
    )
  , ( "tuple_type_def.txt"
    , (parse_and_ret_res_str :: ParseToResStr TupleTypeDef) .> extract_res_str
    )
  , ( "or_type_def.txt"
    , (parse_and_ret_res_str :: ParseToResStr OrTypeDef) .> extract_res_str
    )
  , ( "type_nickname.txt"
    , (parse_and_ret_res_str :: ParseToResStr TypeNickname) .> extract_res_str
    )
  , ( "atom_prop_def.txt"
    , (parse_and_ret_res_str :: ParseToResStr AtomPropDef) .> extract_res_str
    )
  , ( "renaming_prop_def.txt"
    , (parse_and_ret_res_str :: ParseToResStr RenamingPropDef) .>
      extract_res_str
    )
  , ( "type_theo.txt"
    , (parse_and_ret_res_str :: ParseToResStr TypeTheo) .> extract_res_str
    )
  ]

-- For fast vim navigation
-- Parsers.hs
-- ShowInstances.hs
-- ASTTypes.hs
