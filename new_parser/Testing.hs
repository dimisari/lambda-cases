{-# LANGUAGE LambdaCase #-}

module Testing where

import Text.Parsec (runParser, eof, ParseError)

import System.Directory
import Data.List.Split

import ASTTypes
import Parsers
import ShowInstances

-- types
type FileName = String
type ParseResultString = String

type ProgramFileName = FileName
type ProgramStr = String

type ParseFunc = TestExample -> ParseResultString

type FileString = String
type TestExample = String

type ParseToString a = TestExample -> ResultString a
newtype ResultString a = RS ParseResultString

-- paths
[progs_dir, test_exs_dir, in_dir, res_dir] =
  ["programs/", "test_examples/", "parsing_inputs/", "parsing_results/"]
  :: [FilePath]

-- main
main :: IO ()
main =
  listDirectory (in_dir ++ progs_dir) >>= mapM_ read_prog_parse_write_res >>
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
    (parse_and_ret_res_str :: ParseToString Program) .> extract_res_str

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

-- Parse class
class HasParser a => Parse a where
  parse :: String -> Either ParseError a
  parse = runParser (parser <* eof) (0, False) "" 

-- Parse_To_String class
class (Parse a, Show a) => Parse_To_String a where
  parse_and_ret_res_str :: ParseToString a
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
    , (parse_and_ret_res_str :: ParseToString Literal) .> extract_res_str
    )
  , ( "identifiers.txt"
    , (parse_and_ret_res_str :: ParseToString Identifier) .> extract_res_str
    )
  , ( "paren_expr.txt"
    , (parse_and_ret_res_str :: ParseToString ParenExpr) .> extract_res_str
    )
  , ( "tuple.txt"
    , (parse_and_ret_res_str :: ParseToString Tuple) .> extract_res_str
    )
  , ( "big_tuple.txt"
    , (parse_and_ret_res_str :: ParseToString BigTuple) .> extract_res_str
    )
  , ( "list.txt"
    , (parse_and_ret_res_str :: ParseToString List) .> extract_res_str
    )
  , ( "big_list.txt"
    , (parse_and_ret_res_str :: ParseToString BigList) .> extract_res_str
    )
  , ( "paren_func_app.txt"
    , (parse_and_ret_res_str :: ParseToString ParenFuncApp) .> extract_res_str
    )
  , ( "prefix_func_app.txt"
    , (parse_and_ret_res_str :: ParseToString PreFuncApp) .> extract_res_str
    )
  , ( "postfix_func_app.txt"
    , (parse_and_ret_res_str :: ParseToString PostFuncApp) .> extract_res_str
    )
  , ( "line_op_expr.txt"
    , (parse_and_ret_res_str :: ParseToString LineOpExpr) .> extract_res_str
    )
  , ( "big_op_expr.txt"
    , (parse_and_ret_res_str :: ParseToString BigOpExpr) .> extract_res_str
    )
  , ( "line_func_expr.txt"
    , (parse_and_ret_res_str :: ParseToString LineFuncExpr) .> extract_res_str
    )
  , ( "big_func_expr.txt"
    , (parse_and_ret_res_str :: ParseToString BigFuncExpr) .> extract_res_str
    )
  , ( "cases_func_expr.txt"
    , (parse_and_ret_res_str :: ParseToString CasesFuncExpr) .> extract_res_str
    )
  , ( "value_def.txt"
    , (parse_and_ret_res_str :: ParseToString ValueDef) .> extract_res_str
    )
  , ( "grouped_val_defs.txt"
    , (parse_and_ret_res_str :: ParseToString GroupedValueDefs) .>
      extract_res_str
    )
  , ( "where_expr.txt"
    , (parse_and_ret_res_str :: ParseToString WhereExpr) .> extract_res_str
    )
  , ( "type_id.txt"
    , (parse_and_ret_res_str :: ParseToString TypeId) .> extract_res_str
    )
  , ( "type_var.txt"
    , (parse_and_ret_res_str :: ParseToString TypeVar) .> extract_res_str
    )
  , ( "func_type.txt"
    , (parse_and_ret_res_str :: ParseToString FuncType) .> extract_res_str
    )
  , ( "prod_type.txt"
    , (parse_and_ret_res_str :: ParseToString ProdType) .> extract_res_str
    )
  , ( "type_app.txt"
    , (parse_and_ret_res_str :: ParseToString TypeApp) .> extract_res_str
    )
  , ( "cond_type.txt"
    , (parse_and_ret_res_str :: ParseToString Type) .> extract_res_str
    )
  , ( "tuple_type_def.txt"
    , (parse_and_ret_res_str :: ParseToString TupleTypeDef) .> extract_res_str
    )
  , ( "or_type_def.txt"
    , (parse_and_ret_res_str :: ParseToString OrTypeDef) .> extract_res_str
    )
  , ( "type_nickname.txt"
    , (parse_and_ret_res_str :: ParseToString TypeNickname) .> extract_res_str
    )
  , ( "atom_prop_def.txt"
    , (parse_and_ret_res_str :: ParseToString AtomPropDef) .> extract_res_str
    )
  , ( "renaming_prop_def.txt"
    , (parse_and_ret_res_str :: ParseToString RenamingPropDef) .>
      extract_res_str
    )
  , ( "type_theo.txt"
    , (parse_and_ret_res_str :: ParseToString TypeTheo) .> extract_res_str
    )
  ]

-- instances. they use the default implementations above
-- why not automated Haskell?
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
instance Parse LineOpExpr
instance Parse BigOpExpr
instance Parse LineFuncExpr
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

instance Parse Program

instance Parse_To_String Literal
instance Parse_To_String Identifier
instance Parse_To_String ParenExpr
instance Parse_To_String Tuple
instance Parse_To_String BigTuple
instance Parse_To_String List
instance Parse_To_String BigList
instance Parse_To_String ParenFuncApp
instance Parse_To_String PreFuncApp
instance Parse_To_String PostFuncApp
instance Parse_To_String LineOpExpr
instance Parse_To_String BigOpExpr
instance Parse_To_String LineFuncExpr
instance Parse_To_String BigFuncExpr
instance Parse_To_String CasesFuncExpr
instance Parse_To_String ValueDef
instance Parse_To_String GroupedValueDefs
instance Parse_To_String WhereExpr

instance Parse_To_String TypeId
instance Parse_To_String TypeVar
instance Parse_To_String FuncType
instance Parse_To_String ProdType
instance Parse_To_String TypeApp
instance Parse_To_String Type -- CondType
instance Parse_To_String TupleTypeDef
instance Parse_To_String OrTypeDef
instance Parse_To_String TypeNickname
instance Parse_To_String AtomPropDef
instance Parse_To_String RenamingPropDef
instance Parse_To_String TypeTheo

instance Parse_To_String Program

-- For fast vim navigation
-- Parsers.hs
-- ShowInstances.hs
-- ASTTypes.hs
