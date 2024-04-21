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

type ExampleParseFunc = TestExample -> ParseResultString

type ParseFunc a = String -> ParseResStr a

newtype ParseResStr a = RS ParseResultString

-- parse_results_dir
parse_results_dir = "../parsing_results/"
  :: FilePath

-- main
main :: IO ()
main =
  list_progs >>= mapM_ parse_prog >>
  mapM_ parse_example example_parse_func_pairs

-- parse_prog
parse_prog :: ProgramFileName -> IO ()
parse_prog pfn =
  read_prog pfn $> parse_prog_str >>= write_parse_res
  where
  parse_prog_str :: ProgramStr -> ParseResultString
  parse_prog_str = (parse_func :: ParseFunc Program) .> extract_res_str

  write_parse_res :: ParseResultString -> IO ()
  write_parse_res = writeFile $ parse_results_dir ++ progs_dir ++ pfn

-- parse_example
parse_example :: (FileName, ExampleParseFunc) -> IO ()
parse_example (file_name, parse_func) =
  read_examples file_name $> concatMap parse_func >>= write_parse_res
  where
  write_parse_res :: FileString -> IO ()
  write_parse_res = writeFile $ parse_results_dir ++ test_exs_dir ++ file_name

-- parse_func
parse_func :: (HasParser a, Show a) => ParseFunc a
parse_func = parse .> res_to_str

res_to_str :: Show a => Either ParseError a -> ParseResStr a
res_to_str = RS . (++ "\n\n") . \case
  Left err -> "Error :( ==>" ++ show err
  Right res -> "Parsed :) ==>\n" ++ show res

extract_res_str :: ParseResStr a -> ParseResultString
extract_res_str = \(RS s) -> s

-- example_parse_func_pairs
example_parse_func_pairs :: [(FileName, ExampleParseFunc)]
example_parse_func_pairs =
  [ ( "literals.txt"
    , (parse_func :: ParseFunc Literal) .> extract_res_str
    )
  , ( "identifiers.txt"
    , (parse_func :: ParseFunc Identifier) .> extract_res_str
    )
  , ( "paren_expr.txt"
    , (parse_func :: ParseFunc ParenExpr) .> extract_res_str
    )
  , ( "tuple.txt"
    , (parse_func :: ParseFunc Tuple) .> extract_res_str
    )
  , ( "big_tuple.txt"
    , (parse_func :: ParseFunc BigTuple) .> extract_res_str
    )
  , ( "list.txt"
    , (parse_func :: ParseFunc List) .> extract_res_str
    )
  , ( "big_list.txt"
    , (parse_func :: ParseFunc BigList) .> extract_res_str
    )
  , ( "paren_func_app.txt"
    , (parse_func :: ParseFunc ParenFuncAppOrId) .> extract_res_str
    )
  , ( "prefix_func_app.txt"
    , (parse_func :: ParseFunc PreFuncApp) .> extract_res_str
    )
  , ( "postfix_func_app.txt"
    , (parse_func :: ParseFunc PostFuncApp) .> extract_res_str
    )
  , ( "line_op_expr.txt"
    , (parse_func :: ParseFunc LineOpExpr) .> extract_res_str
    )
  , ( "big_op_expr.txt"
    , (parse_func :: ParseFunc BigOpExpr) .> extract_res_str
    )
  , ( "line_func_expr.txt"
    , (parse_func :: ParseFunc LineFuncExpr) .> extract_res_str
    )
  , ( "big_func_expr.txt"
    , (parse_func :: ParseFunc BigFuncExpr) .> extract_res_str
    )
  , ( "cases_func_expr.txt"
    , (parse_func :: ParseFunc CasesFuncExpr) .> extract_res_str
    )
  , ( "value_def.txt"
    , (parse_func :: ParseFunc ValueDef) .> extract_res_str
    )
  , ( "grouped_val_defs.txt"
    , (parse_func :: ParseFunc GroupedValueDefs) .> extract_res_str
    )
  , ( "where_expr.txt"
    , (parse_func :: ParseFunc WhereExpr) .> extract_res_str
    )
  , ( "type_id.txt"
    , (parse_func :: ParseFunc TypeId) .> extract_res_str
    )
  , ( "type_var.txt"
    , (parse_func :: ParseFunc TypeVar) .> extract_res_str
    )
  , ( "func_type.txt"
    , (parse_func :: ParseFunc FuncType) .> extract_res_str
    )
  , ( "prod_type.txt"
    , (parse_func :: ParseFunc ProdType) .> extract_res_str
    )
  , ( "type_app.txt"
    , (parse_func :: ParseFunc TypeApp) .> extract_res_str
    )
  , ( "cond_type.txt"
    , (parse_func :: ParseFunc Type) .> extract_res_str
    )
  , ( "tuple_type_def.txt"
    , (parse_func :: ParseFunc TupleTypeDef) .> extract_res_str
    )
  , ( "or_type_def.txt"
    , (parse_func :: ParseFunc OrTypeDef) .> extract_res_str
    )
  , ( "type_nickname.txt"
    , (parse_func :: ParseFunc TypeNickname) .> extract_res_str
    )
  , ( "atom_prop_def.txt"
    , (parse_func :: ParseFunc AtomPropDef) .> extract_res_str
    )
  , ( "renaming_prop_def.txt"
    , (parse_func :: ParseFunc RenamingPropDef) .> extract_res_str
    )
  , ( "type_theo.txt"
    , (parse_func :: ParseFunc TypeTheo) .> extract_res_str
    )
  ]

-- For fast vim navigation
-- ShowInstances.hs
-- ASTTypes.hs
-- Parsing/AST.hs
