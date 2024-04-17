{-# LANGUAGE LambdaCase, FlexibleInstances #-}

module Generation.Test where

import System.Process
import Text.Parsec (runParser, eof, ParseError)

import Data.List.Split

import Helpers
import ASTTypes

import Parsing.AST

import Generation.TypesAndHelpers
import Generation.FieldIds
import Generation.DotChangePreprocess
import Generation.AST

-- types
type GenHsFunc = TestExample -> Haskell

type Lcases = String

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
  readFile in_path >>= prog_gen_hs .> (imports ++) .> writeFile out_path
  where
  (in_path, out_path) =
    (in_dir ++ progs_dir ++ pfn, res_dir ++ progs_dir ++ change_extension pfn)
    :: (FilePath, FilePath)

  prog_gen_hs :: Lcases -> Haskell
  prog_gen_hs =
    parse .> parse_res_to_hs
    where
    parse_res_to_hs :: Either ParseError Program -> Haskell
    parse_res_to_hs = \case
      Left err -> "Error :( ==>" ++ show err ++ "\n\n"
      Right prog ->
        change_program_if_needed (prog, get_field_ids prog) &> \new_prog ->
        to_haskell new_prog ++ "\n\n"

  imports :: Haskell
  imports = concatMap (\im_n -> "import " ++ im_n ++ "\n") import_names ++ "\n"

  import_names :: [Haskell]
  import_names =
    ["Prelude hiding (IO)", "Haskell.Predefined", "Haskell.OpsInHaskell"]

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

-- parse, generate_hs
parse :: HasParser a => String -> Either ParseError a
parse = runParser (parser <* eof) (0, False) ""

generate_hs :: (HasParser a, ToHaskell a) => GenerateHs a
generate_hs =
  parse .> parse_res_to_hs
  where
  parse_res_to_hs :: ToHaskell a => Either ParseError a -> ResultString a
  parse_res_to_hs = RS . \case
    Left err -> "Error :( ==>" ++ show err ++ "\n\n"
    Right res -> to_haskell res ++ "\n\n"

res_to_str_lit :: Either ParseError Literal -> ResultString Literal
res_to_str_lit = RS . \case
  Left err -> "Error :( ==>" ++ show err ++ "\n\n"
  Right lit -> to_haskell (NoAnnotation, lit) ++ "\n\n"

-- test_exs_file_name_parse_func_pairs
extract_res_str :: ResultString a -> Haskell
extract_res_str = \(RS s) -> s

test_exs_file_name_parse_func_pairs :: [(FileName, GenHsFunc)]
test_exs_file_name_parse_func_pairs =
  [ ( "literals.txt", parse .> res_to_str_lit .> extract_res_str)
  , ( "identifiers.txt"
    , (generate_hs :: GenerateHs Identifier) .> extract_res_str
    )
  , ( "paren_expr.txt"
    , (generate_hs :: GenerateHs ParenExpr) .>
      extract_res_str
    )
  , ( "tuple.txt"
    , (generate_hs :: GenerateHs Tuple) .>
      extract_res_str
    )
  , ( "big_tuple.txt"
    , (generate_hs :: GenerateHs (THWIL BigTuple)) .> extract_res_str
    )
  , ( "list.txt"
    , (generate_hs :: GenerateHs List) .>
      extract_res_str
    )
  , ( "big_list.txt"
    , (generate_hs :: GenerateHs (THWIL BigList)) .> extract_res_str
    )
  , ( "paren_func_app.txt"
    , (generate_hs :: GenerateHs ParenFuncAppOrId)
      .>
      extract_res_str
    )
  , ( "prefix_func_app.txt"
    , (generate_hs :: GenerateHs PreFuncApp) .>
      extract_res_str
    )
  , ( "postfix_func_app.txt"
    , (generate_hs :: GenerateHs PostFuncApp) .> extract_res_str
    )
  , ( "line_op_expr.txt"
    , (generate_hs :: GenerateHs LineOpExpr) .>
      extract_res_str
    )
  , ( "big_op_expr.txt"
    , (generate_hs :: GenerateHs (THWIL BigOpExpr)) .>
      extract_res_str
    )
  , ( "line_func_expr.txt"
    , (generate_hs :: GenerateHs LineFuncExpr) .>
      extract_res_str
    )
  , ( "big_func_expr.txt"
    , (generate_hs :: GenerateHs (THWIL BigFuncExpr)) .>
      extract_res_str
    )
  , ( "cases_func_expr.txt"
    , (generate_hs :: GenerateHs (THWIL CasesFuncExpr)) .>
      extract_res_str
    )
  , ( "value_def.txt"
    , (generate_hs :: GenerateHs (THWIL ValueDef)) .> extract_res_str
    )
  , ( "grouped_val_defs.txt"
    , (generate_hs :: GenerateHs (THWIL GroupedValueDefs)) .>
      extract_res_str
    )
  , ( "where_expr.txt"
    , (generate_hs :: GenerateHs (THWIL WhereExpr)) .>
      extract_res_str
    )
  , ( "type_id.txt"
    , (generate_hs :: GenerateHs TypeId) .> extract_res_str
    )
  , ( "type_var.txt"
    , (generate_hs :: GenerateHs TypeVar) .> extract_res_str
    )
  , ( "func_type.txt"
    , (generate_hs :: GenerateHs FuncType) .> extract_res_str
    )
  , ( "prod_type.txt"
    , (generate_hs :: GenerateHs ProdType) .> extract_res_str
    )
  , ( "type_app.txt"
    , (generate_hs :: GenerateHs TypeApp) .> extract_res_str
    )
  , ( "cond_type.txt"
    , (generate_hs :: GenerateHs Type) .> extract_res_str
    )
  , ( "tuple_type_def.txt"
    , (generate_hs :: GenerateHs TupleTypeDef) .> extract_res_str
    )
  , ( "or_type_def.txt"
    , (generate_hs :: GenerateHs OrTypeDef) .> extract_res_str
    )
  , ( "type_nickname.txt"
    , (generate_hs :: GenerateHs TypeNickname) .> extract_res_str
    )
  , ( "atom_prop_def.txt"
    , (generate_hs :: GenerateHs AtomPropDef) .> extract_res_str
    )
  , ( "renaming_prop_def.txt"
    , (generate_hs :: GenerateHs RenamingPropDef) .>
      extract_res_str
    )
  , ( "type_theo.txt"
    , (generate_hs :: GenerateHs TypeTheo) .> extract_res_str
    )
  ]

-- to have ToHaskell from ToHsWithIndentLvl and also HasParser
-- for generate_hs
newtype THWIL a = THWIL a

instance ToHsWithIndentLvl a => ToHaskell (THWIL a) where
   to_haskell (THWIL a) = to_hs_wil a &> run_generator

instance HasParser a => HasParser (THWIL a) where
   parser = THWIL <$> parser

-- For fast vim navigation
-- ASTTypes.hs
-- TypesAndHelpers.hs
-- AST.hs
