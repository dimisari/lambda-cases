{-# LANGUAGE LambdaCase,
TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}

module Testing where

import Text.Parsec (runParser, eof, ParseError)

import System.Directory
import Data.List
import Data.List.Split

import ASTTypes
import Parsers
import ShowInstances

-- helpers

(.>) = flip (.)
(%>) = flip ($)

type FileName = String
type FileString = String
type TestExample = String
type ParseResult = String
type ParseFunc = TestExample -> ParseResult

-- main

main :: IO ()
main =
  listDirectory "test_examples/" >>= run_parse_funcs_on_all_files
  where
  run_parse_funcs_on_all_files :: [ FileName ] -> IO ()
  run_parse_funcs_on_all_files = mapM_ run_parse_func_for_file

run_parse_func_for_file :: FileName -> IO ()
run_parse_func_for_file = \file_name ->
  case lookup file_name file_name_parse_func_pairs of
    Nothing ->
      putStrLn $ "Error: Could not find parsing func for file_name: " ++ file_name
    Just parse_func ->
      readFile input_path >>= in_str_to_out_str .> writeFile output_path
      where
      input_path :: FilePath
      input_path = "test_examples/" ++ file_name

      output_path :: FilePath
      output_path = "parsing_results/" ++ file_name

      in_str_to_out_str :: FileString -> FileString
      in_str_to_out_str = file_string_to_examples .> concatMap parse_func

      file_string_to_examples :: FileString -> [ TestExample ]
      file_string_to_examples = endBy "\n#\n"

-- Parse class and result to string

class HasParser a => Parse a where
  parse :: TestExample -> Either ParseError a
  parse = runParser (parser <* eof) 0 "" 

parse_result_to_string :: Show a => Either ParseError a -> ParseResult
parse_result_to_string = \case
  Left err -> "Error :( ==>" ++ (show err %> dropWhile (/= '\n')) ++ "\n\n"
  Right res -> "Parsed :) ==>\n" ++ show res ++ "\n\n"

-- "Parse and result to string" functions for each type to be parsed

file_name_parse_func_pairs :: [(FileName, ParseFunc)]
file_name_parse_func_pairs =
  [ ("literals.txt", parse_lit_and_ret_res_str)
  , ("identifiers.txt", parse_id_and_ret_res_str)
  , ("paren_expr.txt", parse_paren_expr_and_ret_res_str)
  , ("tuple.txt", parse_tuple_and_ret_res_str)
  ]

parse_lit_and_ret_res_str :: ParseFunc
parse_lit_and_ret_res_str =
  \test_example ->
  parse_result_to_string (parse test_example :: Either ParseError Literal)

parse_id_and_ret_res_str :: ParseFunc
parse_id_and_ret_res_str =
  \test_example ->
  parse_result_to_string (parse test_example :: Either ParseError Identifier)

parse_paren_expr_and_ret_res_str :: ParseFunc
parse_paren_expr_and_ret_res_str =
  \test_example ->
  parse_result_to_string (parse test_example :: Either ParseError ParenExpr)

parse_tuple_and_ret_res_str :: ParseFunc
parse_tuple_and_ret_res_str =
  \test_example ->
  parse_result_to_string (parse test_example :: Either ParseError Tuple)

-- Parse class instances (why not automated Haskell?)

instance Parse Literal
instance Parse Identifier
instance Parse ParenExpr
instance Parse Tuple
instance Parse BigTuple
instance Parse List
instance Parse BigList
instance Parse ParenFuncApp
instance Parse PreFuncApp
instance Parse PostFuncApp
instance Parse OpExpr
instance Parse SimpleOpExpr
instance Parse BigOpExpr
instance Parse CasesOpExpr
instance Parse FuncExpr
instance Parse SimpleFuncExpr
instance Parse BigFuncExpr
instance Parse CasesFuncExpr
instance Parse ValueDef
instance Parse GroupedValueDefs

instance Parse Type
instance Parse FuncType
instance Parse ProdType
instance Parse TypeApp
instance Parse TypeDef
instance Parse TupleTypeDef
instance Parse OrTypeDef
instance Parse TypeNickname
instance Parse TypePropDef
instance Parse TypeTheo
