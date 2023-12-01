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
  mapM_ run_parse_func_for_file file_name_parse_func_pairs

run_parse_func_for_file :: (FileName, ParseFunc) -> IO ()
run_parse_func_for_file (file_name, parse_func) =
  readFile input_path >>= in_str_to_out_str .> writeFile output_path
--   readFile input_path >>=
--   file_string_to_examples .> concatMap ((++ "\n\n") . show) .> writeFile output_path
  where
  input_path :: FilePath
  input_path = "test_examples/" ++ file_name

  output_path :: FilePath
  output_path = "parsing_results/" ++ file_name

  in_str_to_out_str :: FileString -> FileString
  in_str_to_out_str = file_string_to_examples .> concatMap parse_func

  file_string_to_examples :: FileString -> [ TestExample ]
  file_string_to_examples = endBy "#\n\n"

-- Parse class and result to string

class HasParser a => Parse a where
  parse :: TestExample -> Either ParseError a
  parse = runParser (parser <* eof) (0, False) "" 

parse_result_to_string :: Show a => Either ParseError a -> ParseResult
parse_result_to_string = \case
  Left err -> "Error :( ==>" ++ (show err {- %> dropWhile (/= '\n') -}) ++ "\n\n"
  Right res -> "Parsed :) ==>\n" ++ show res ++ "\n\n"

-- "Parse and result to string" functions for each type to be parsed

file_name_parse_func_pairs :: [(FileName, ParseFunc)]
file_name_parse_func_pairs =
  [ ("literals.txt", parse_lit_and_ret_res_str)
  , ("identifiers.txt", parse_id_and_ret_res_str)
  , ("paren_expr.txt", parse_paren_expr_and_ret_res_str)
  , ("tuple.txt", parse_tuple_and_ret_res_str)
  , ("big_tuple.txt", parse_big_tuple_and_ret_res_str)
  , ("list.txt", parse_list_and_ret_res_str)
  , ("big_list.txt", parse_big_list_and_ret_res_str)
  , ("paren_func_app.txt", parse_paren_func_app_and_ret_res_str)
  , ("prefix_func_app.txt", parse_pre_func_app_and_ret_res_str)
  , ("postfix_func_app.txt", parse_post_func_app_and_ret_res_str)
  , ("simple_op_expr.txt", parse_simple_op_expr_and_ret_res_str)
  , ("big_op_expr.txt", parse_big_op_expr_and_ret_res_str)
  , ("simple_func_expr.txt", parse_simple_func_expr_and_ret_res_str)
  , ("big_func_expr.txt", parse_big_func_expr_and_ret_res_str)
  , ("cases_func_expr.txt", parse_cases_func_expr_and_ret_res_str)
  , ("value_def.txt", parse_value_def_and_ret_res_str)
  , ("grouped_val_defs.txt", parse_grouped_val_defs_and_ret_res_str)
  , ("where_expr.txt", parse_where_expr_and_ret_res_str)
  , ("type_id.txt", parse_type_id_and_ret_res_str)
  , ("type_var.txt", parse_type_var_and_ret_res_str)
  , ("func_type.txt", parse_func_type_and_ret_res_str)
  , ("prod_type.txt", parse_prod_type_and_ret_res_str)
  , ("type_app.txt", parse_type_app_and_ret_res_str)
  , ("cond_type.txt", parse_cond_type_and_ret_res_str)
  , ("tuple_type_def.txt", parse_tuple_type_def_and_ret_res_str)
  , ("or_type_def.txt", parse_or_type_def_and_ret_res_str)
  , ("type_nickname.txt", parse_type_nickname_and_ret_res_str)
  , ("atom_prop_def.txt", parse_atom_prop_def_and_ret_res_str)
  , ("renaming_prop_def.txt", parse_renaming_prop_def_and_ret_res_str)
  , ("type_theo.txt", parse_type_theo_and_ret_res_str)
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

parse_big_tuple_and_ret_res_str :: ParseFunc
parse_big_tuple_and_ret_res_str =
  \test_example ->
  parse_result_to_string (parse test_example :: Either ParseError BigTuple)

parse_list_and_ret_res_str :: ParseFunc
parse_list_and_ret_res_str =
  \test_example ->
  parse_result_to_string (parse test_example :: Either ParseError List)

parse_big_list_and_ret_res_str :: ParseFunc
parse_big_list_and_ret_res_str =
  \test_example ->
  parse_result_to_string (parse test_example :: Either ParseError BigList)

parse_paren_func_app_and_ret_res_str :: ParseFunc
parse_paren_func_app_and_ret_res_str =
  \test_example ->
  parse_result_to_string (parse test_example :: Either ParseError ParenFuncApp)

parse_pre_func_app_and_ret_res_str :: ParseFunc
parse_pre_func_app_and_ret_res_str =
  \test_example ->
  parse_result_to_string (parse test_example :: Either ParseError PreFuncApp)

parse_post_func_app_and_ret_res_str :: ParseFunc
parse_post_func_app_and_ret_res_str =
  \test_example ->
  parse_result_to_string (parse test_example :: Either ParseError PostFuncApp)

parse_simple_op_expr_and_ret_res_str :: ParseFunc
parse_simple_op_expr_and_ret_res_str =
  \test_example ->
  parse_result_to_string (parse test_example :: Either ParseError SimpleOpExpr)

parse_big_op_expr_and_ret_res_str :: ParseFunc
parse_big_op_expr_and_ret_res_str =
  \test_example ->
  parse_result_to_string (parse test_example :: Either ParseError BigOpExpr)

parse_simple_func_expr_and_ret_res_str :: ParseFunc
parse_simple_func_expr_and_ret_res_str =
  \test_example ->
  parse_result_to_string (parse test_example :: Either ParseError SimpleFuncExpr)

parse_big_func_expr_and_ret_res_str :: ParseFunc
parse_big_func_expr_and_ret_res_str =
  \test_example ->
  parse_result_to_string (parse test_example :: Either ParseError BigFuncExpr)

parse_cases_func_expr_and_ret_res_str :: ParseFunc
parse_cases_func_expr_and_ret_res_str =
  \test_example ->
  parse_result_to_string (parse test_example :: Either ParseError CasesFuncExpr)

parse_value_def_and_ret_res_str :: ParseFunc
parse_value_def_and_ret_res_str =
  \test_example ->
  parse_result_to_string (parse test_example :: Either ParseError ValueDef)

parse_grouped_val_defs_and_ret_res_str :: ParseFunc
parse_grouped_val_defs_and_ret_res_str =
  \test_example ->
  parse_result_to_string (parse test_example :: Either ParseError GroupedValueDefs)

parse_where_expr_and_ret_res_str :: ParseFunc
parse_where_expr_and_ret_res_str =
  \test_example ->
  parse_result_to_string (parse test_example :: Either ParseError WhereExpr)

parse_type_id_and_ret_res_str :: ParseFunc
parse_type_id_and_ret_res_str =
  \test_example ->
  parse_result_to_string (parse test_example :: Either ParseError TypeId)

parse_type_var_and_ret_res_str :: ParseFunc
parse_type_var_and_ret_res_str =
  \test_example ->
  parse_result_to_string (parse test_example :: Either ParseError TypeVar)

parse_func_type_and_ret_res_str :: ParseFunc
parse_func_type_and_ret_res_str =
  \test_example ->
  parse_result_to_string (parse test_example :: Either ParseError FuncType)

parse_prod_type_and_ret_res_str :: ParseFunc
parse_prod_type_and_ret_res_str =
  \test_example ->
  parse_result_to_string (parse test_example :: Either ParseError ProdType)

parse_type_app_and_ret_res_str :: ParseFunc
parse_type_app_and_ret_res_str =
  \test_example ->
  parse_result_to_string (parse test_example :: Either ParseError TypeApp)

parse_cond_type_and_ret_res_str :: ParseFunc
parse_cond_type_and_ret_res_str =
  \test_example ->
  parse_result_to_string (parse test_example :: Either ParseError Type )

parse_tuple_type_def_and_ret_res_str :: ParseFunc
parse_tuple_type_def_and_ret_res_str =
  \test_example ->
  parse_result_to_string (parse test_example :: Either ParseError TupleTypeDef)

parse_or_type_def_and_ret_res_str :: ParseFunc
parse_or_type_def_and_ret_res_str =
  \test_example ->
  parse_result_to_string (parse test_example :: Either ParseError OrTypeDef)

parse_type_nickname_and_ret_res_str :: ParseFunc
parse_type_nickname_and_ret_res_str =
  \test_example ->
  parse_result_to_string (parse test_example :: Either ParseError TypeNickname)

parse_atom_prop_def_and_ret_res_str :: ParseFunc
parse_atom_prop_def_and_ret_res_str =
  \test_example ->
  parse_result_to_string (parse test_example :: Either ParseError AtomPropDef)

parse_renaming_prop_def_and_ret_res_str :: ParseFunc
parse_renaming_prop_def_and_ret_res_str =
  \test_example ->
  parse_result_to_string (parse test_example :: Either ParseError RenamingPropDef)

parse_type_theo_and_ret_res_str :: ParseFunc
parse_type_theo_and_ret_res_str =
  \test_example ->
  parse_result_to_string (parse test_example :: Either ParseError TypeTheo)

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
instance Parse SimpleOpExpr
instance Parse BigOpExpr
instance Parse SimpleFuncExpr
instance Parse BigFuncExpr
instance Parse CasesFuncExpr
instance Parse ValueDef
instance Parse GroupedValueDefs
instance Parse WhereExpr

instance Parse TypeId
instance Parse TypeVar
instance Parse FuncType
instance Parse ProdType
instance Parse TypeApp
instance Parse Type -- CondType
instance Parse TupleTypeDef
instance Parse OrTypeDef
instance Parse TypeNickname
instance Parse AtomPropDef
instance Parse RenamingPropDef
instance Parse TypeTheo
