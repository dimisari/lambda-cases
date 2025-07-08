{-
This file reads all the examples from the text files of the
/test/inputs/grammar_rules directory, compiles them and writes the compiled
version into identically named files in the /test/outputs/grammar_rules
directory
-}

{-# language LambdaCase, FlexibleInstances #-}

module Main where

-- imports

import System.Environment (getArgs)
import Text.Parsec (runParser, eof, ParseError)
import Data.List.Split (endBy)

import Helpers
import ASTTypes

import Parsing.AST

import Generation.TypesAndHelpers
import Generation.AST

-- types

type FileString = String
type TestExample = String

type CompileExFunc = TestExample -> String

type Compile a = Lcases -> ResultString a
newtype ResultString a = RS String

-- main

main :: IO ()
main = mapM_ compile_examples file_name_compile_func_pairs

-- compile the examples of a file

compile_examples :: (FileName, CompileExFunc) -> IO ()
compile_examples (file_name, comp_ex_func) =
  read_examples file_name >$> concatMap comp_ex_func >>= \ex_outs ->
  get_out_path >>= \out_path ->
  writeFile out_path ex_outs
  where
  get_out_path :: IO FilePath
  get_out_path = get_test_outputs_path >$> (++ make_extension_hs file_name)

get_test_outputs_path :: IO FilePath
get_test_outputs_path = getArgs >$> (!!1) >$> (++ "/")

compile_example_func :: (HasParser a, ToHaskell a) => Compile a
compile_example_func = parse .> parse_res_to_final_res

parse_res_to_final_res :: ToHaskell a => Either ParseError a -> ResultString a
parse_res_to_final_res = RS . (++ "\n\n") . \case
  Left err -> "Error :( ==>" ++ show err
  Right res -> to_haskell res

extract_res_str :: ResultString a -> Haskell
extract_res_str = \(RS s) -> s

-- Reading the examples from a file

read_examples :: FileName -> IO [FileString]
read_examples = \file_name -> read_exs_file file_name >$> file_str_to_examples

read_exs_file :: FileName -> IO FileString
read_exs_file = \file_name ->
  get_test_inputs_path >$> (++ file_name) >>= readFile

get_test_inputs_path :: IO FilePath
get_test_inputs_path = getArgs >$> head >$> (++ "/")

file_str_to_examples :: FileString -> [ TestExample ]
file_str_to_examples = endBy "#\n\n"

-- Pairs of file names and the correcsponding compile function for
-- each example in the file

file_name_compile_func_pairs :: [(FileName, CompileExFunc)]
file_name_compile_func_pairs =
  [ ( "literals.txt"
    , (compile_example_func :: Compile (NeedsAnnotBool, Literal)) .>
      extract_res_str
    )
  , ( "identifiers.txt"
    , (compile_example_func :: Compile Identifier) .> extract_res_str
    )
  , ( "paren_expr.txt"
    , (compile_example_func :: Compile ParenExpr) .> extract_res_str
    )
  , ( "tuple.txt"
    , (compile_example_func :: Compile Tuple) .> extract_res_str
    )
  , ( "big_tuple.txt"
    , (compile_example_func :: Compile (THWIL BigTuple)) .> extract_res_str
    )
  , ( "list.txt"
    , (compile_example_func :: Compile List) .> extract_res_str
    )
  , ( "big_list.txt"
    , (compile_example_func :: Compile (THWIL BigList)) .> extract_res_str
    )
  , ( "paren_func_app.txt"
    , (compile_example_func :: Compile ParenFuncAppOrId) .> extract_res_str
    )
  , ( "prefix_func_app.txt"
    , (compile_example_func :: Compile PreFuncApp) .> extract_res_str
    )
  , ( "postfix_func_app.txt"
    , (compile_example_func :: Compile PostFuncApp) .> extract_res_str
    )
  , ( "line_op_expr.txt"
    , (compile_example_func :: Compile LineOpExpr) .> extract_res_str
    )
  , ( "big_op_expr.txt"
    , (compile_example_func :: Compile (THWIL BigOpExpr)) .> extract_res_str
    )
  , ( "line_func_expr.txt"
    , (compile_example_func :: Compile LineFuncExpr) .> extract_res_str
    )
  , ( "big_func_expr.txt"
    , ( compile_example_func ::
        Compile (THWIL (BigFuncExpr, PossiblyWhereExpr))
      ) .> extract_res_str
    )
  , ( "cases_func_expr.txt"
    , ( compile_example_func ::
        Compile (THWIL (CasesFuncExpr, PossiblyWhereExpr))
      ) .> extract_res_str
    )
  , ( "value_def.txt"
    , (compile_example_func :: Compile (THWIL ValueDef)) .> extract_res_str
    )
  , ( "grouped_val_defs.txt"
    , (compile_example_func :: Compile (THWIL GroupedValueDefs)) .>
      extract_res_str
    )
  , ( "where_expr.txt"
    , (compile_example_func :: Compile (THWIL WhereExpr)) .> extract_res_str
    )
  , ( "type_id.txt"
    , (compile_example_func :: Compile TypeId) .> extract_res_str
    )
  , ( "func_type.txt"
    , (compile_example_func :: Compile FuncType) .> extract_res_str
    )
  , ( "prod_type.txt"
    , (compile_example_func :: Compile ProdType) .> extract_res_str
    )
  , ( "type_app.txt"
    , (compile_example_func :: Compile (NeedsParenBool, TypeAppIdOrAHTV)) .>
      extract_res_str
    )
  , ( "cond_type.txt"
    , (compile_example_func :: Compile Type) .> extract_res_str
    )
  , ( "tuple_type_def.txt"
    , (compile_example_func :: Compile TupleTypeDef) .> extract_res_str
    )
  , ( "or_type_def.txt"
    , (compile_example_func :: Compile OrTypeDef) .> extract_res_str
    )
  , ( "type_nickname.txt"
    , (compile_example_func :: Compile TypeNickname) .> extract_res_str
    )
  , ( "atom_prop_def.txt"
    , (compile_example_func :: Compile AtomPropDef) .> extract_res_str
    )
  , ( "renaming_prop_def.txt"
    , (compile_example_func :: Compile RenamingPropDef) .> extract_res_str
    )
  , ( "type_theo.txt"
    , (compile_example_func :: Compile TypeTheo) .> extract_res_str
    )
  , ( "param_tv.txt"
    , (compile_example_func :: Compile ParamTVar) .> extract_res_str
    )
  , ( "ad_hoc_tv.txt"
    , (compile_example_func :: Compile AdHocTVar) .> extract_res_str
    )
  ]

-- to have ToHaskell from ToHsWithIndentLvl and also HasParser
-- for compile_example_func

newtype THWIL a = THWIL a

instance ToHsWithIndentLvl a => ToHaskell (THWIL a) where
   to_haskell (THWIL a) = to_hs_wil a &> run_generator

instance HasParser a => HasParser (THWIL a) where
   parser = THWIL <$> parser

instance HasParser a => HasParser (a, PossiblyWhereExpr) where
   parser = parser >$> \a -> (a, NoWhereExpr)

instance HasParser a => HasParser (NeedsParenBool, a) where
   parser = parser >$> \a -> (NoParen, a)

instance HasParser a => HasParser (NeedsAnnotBool, a) where
   parser = parser >$> \a -> (NoAnnot, a)

-- For fast vim file navigation:
{-
ASTTypes.hs
-}
