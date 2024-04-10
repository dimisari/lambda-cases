{-# LANGUAGE LambdaCase, FlexibleInstances #-}

module Generation.Test where

import System.Process
import Text.Parsec (runParser, eof, ParseError)

import Data.List.Split

import ASTTypes
import Parsing.AST
import Generation.TypesAndHelpers
import Generation.AST
import Helpers

-- types
type GenHsFunc = TestExample -> Haskell

type GenerateHs a = TestExample -> ResultString a
newtype ResultString a = RS Haskell

-- paths
res_dir = "../hs_gen_results/"
  :: FilePath

-- main
main :: IO ()
main =
  list_progs >>= mapM_ read_prog_parse_write_res >>
  callCommand "./compile.sh" >>
  mapM_ run_parse_func_for_test_exs_file test_exs_file_name_parse_func_pairs

-- helpers
change_extension :: FileName -> FileName
change_extension = takeWhile (/= '.') .> (++ ".hs")
  
-- read_prog_parse_write_res 
read_prog_parse_write_res :: ProgramFileName -> IO ()
read_prog_parse_write_res pfn =
  readFile in_path >>= parse_program .> (imports ++) .> writeFile out_path
  where
  (in_path, out_path) =
    (in_dir ++ progs_dir ++ pfn, res_dir ++ progs_dir ++ change_extension pfn)
    :: (FilePath, FilePath)

  parse_program :: GenHsFunc
  parse_program =
    (parse_and_ret_res_str :: GenerateHs Program) .> extract_res_str

  imports :: Haskell
  imports = concatMap (\im_n -> "import " ++ im_n ++ "\n") import_names ++ "\n"

  import_names :: [Haskell]
  import_names =
    ["Prelude hiding (IO, gcd)", "Haskell.Predefined", "Haskell.OpsInHaskell"]

-- run_parse_func_for_test_exs_file
run_parse_func_for_test_exs_file :: (FileName, GenHsFunc) -> IO ()
run_parse_func_for_test_exs_file (file_name, parse_func) =
  readFile in_path >>= in_str_to_out_str .> writeFile out_path
  where
  (in_path, out_path) =
    ( in_dir ++ test_exs_dir ++ file_name
    , res_dir ++ test_exs_dir ++ change_extension file_name
    )
    :: (FilePath, FilePath)

  in_str_to_out_str :: FileString -> FileString
  in_str_to_out_str = file_string_to_examples .> concatMap parse_func

  file_string_to_examples :: FileString -> [ TestExample ]
  file_string_to_examples = endBy "#\n\n"

-- parse, parse_and_ret_res_str
parse :: HasParser a => String -> Either ParseError a
parse = runParser (parser <* eof) (0, False) "" 

parse_and_ret_res_str :: (HasParser a, ToHaskell a) => GenerateHs a
parse_and_ret_res_str =
  parse .> res_to_str
  where
  res_to_str :: ToHaskell a => Either ParseError a -> ResultString a
  res_to_str = RS <$> \case
    Left err -> "Error :( ==>" ++ show err ++ "\n\n"
    Right res -> to_haskell res ++ "\n\n"

-- test_exs_file_name_parse_func_pairs
extract_res_str :: ResultString a -> Haskell
extract_res_str = \(RS s) -> s

test_exs_file_name_parse_func_pairs :: [(FileName, GenHsFunc)]
test_exs_file_name_parse_func_pairs =
  [ ( "literals.txt"
    , (parse_and_ret_res_str :: GenerateHs Literal) .> extract_res_str
    )
  , ( "identifiers.txt"
    , (parse_and_ret_res_str :: GenerateHs Identifier) .> extract_res_str
    )
  , ( "paren_expr.txt"
    , (parse_and_ret_res_str :: GenerateHs ParenExpr) .> extract_res_str
    )
  , ( "tuple.txt"
    , (parse_and_ret_res_str :: GenerateHs Tuple) .> extract_res_str
    )
  , ( "big_tuple.txt"
    , (parse_and_ret_res_str :: GenerateHs (THWIL BigTuple)) .> extract_res_str
    )
  , ( "list.txt"
    , (parse_and_ret_res_str :: GenerateHs List) .> extract_res_str
    )
  , ( "big_list.txt"
    , (parse_and_ret_res_str :: GenerateHs (THWIL BigList)) .> extract_res_str
    )
  , ( "paren_func_app.txt"
    , (parse_and_ret_res_str :: GenerateHs ParenFuncAppOrId) .> extract_res_str
    )
  , ( "prefix_func_app.txt"
    , (parse_and_ret_res_str :: GenerateHs PreFuncApp) .> extract_res_str
    )
  , ( "postfix_func_app.txt"
    , (parse_and_ret_res_str :: GenerateHs PostFuncApp) .> extract_res_str
    )
  , ( "line_op_expr.txt"
    , (parse_and_ret_res_str :: GenerateHs LineOpExpr) .> extract_res_str
    )
  , ( "big_op_expr.txt"
    , (parse_and_ret_res_str :: GenerateHs (THWIL BigOpExpr)) .>
      extract_res_str
    )
  , ( "line_func_expr.txt"
    , (parse_and_ret_res_str :: GenerateHs LineFuncExpr) .> extract_res_str
    )
  , ( "big_func_expr.txt"
    , (parse_and_ret_res_str :: GenerateHs (THWIL BigFuncExpr)) .>
      extract_res_str
    )
  , ( "cases_func_expr.txt"
    , (parse_and_ret_res_str :: GenerateHs (THWIL CasesFuncExpr)) .>
      extract_res_str
    )
  , ( "value_def.txt"
    , (parse_and_ret_res_str :: GenerateHs (THWIL ValueDef)) .> extract_res_str
    )
  , ( "grouped_val_defs.txt"
    , (parse_and_ret_res_str :: GenerateHs (THWIL GroupedValueDefs)) .>
      extract_res_str
    )
  , ( "where_expr.txt"
    , (parse_and_ret_res_str :: GenerateHs (THWIL WhereExpr)) .>
      extract_res_str
    )
  , ( "type_id.txt"
    , (parse_and_ret_res_str :: GenerateHs TypeId) .> extract_res_str
    )
  , ( "type_var.txt"
    , (parse_and_ret_res_str :: GenerateHs TypeVar) .> extract_res_str
    )
  , ( "func_type.txt"
    , (parse_and_ret_res_str :: GenerateHs FuncType) .> extract_res_str
    )
  , ( "prod_type.txt"
    , (parse_and_ret_res_str :: GenerateHs ProdType) .> extract_res_str
    )
  , ( "type_app.txt"
    , (parse_and_ret_res_str :: GenerateHs TypeApp) .> extract_res_str
    )
  , ( "cond_type.txt"
    , (parse_and_ret_res_str :: GenerateHs Type) .> extract_res_str
    )
  , ( "tuple_type_def.txt"
    , (parse_and_ret_res_str :: GenerateHs TupleTypeDef) .> extract_res_str
    )
  , ( "or_type_def.txt"
    , (parse_and_ret_res_str :: GenerateHs OrTypeDef) .> extract_res_str
    )
  , ( "type_nickname.txt"
    , (parse_and_ret_res_str :: GenerateHs TypeNickname) .> extract_res_str
    )
  , ( "atom_prop_def.txt"
    , (parse_and_ret_res_str :: GenerateHs AtomPropDef) .> extract_res_str
    )
  , ( "renaming_prop_def.txt"
    , (parse_and_ret_res_str :: GenerateHs RenamingPropDef) .>
      extract_res_str
    )
  , ( "type_theo.txt"
    , (parse_and_ret_res_str :: GenerateHs TypeTheo) .> extract_res_str
    )
  ]

-- to have ToHaskell from ToHsWithIndentLvl and also HasParser
-- for parse_and_ret_res_str
newtype THWIL a = THWIL a

instance ToHsWithIndentLvl a => ToHaskell (THWIL a) where
   to_haskell (THWIL a) = to_hs_wil a &> run_generator

instance HasParser a => HasParser (THWIL a) where
   parser = THWIL <$> parser

-- For fast vim navigation
-- ASTTypes.hs
-- Parsers.hs
-- TypesAndHelpers.hs
-- AST.hs
