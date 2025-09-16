{-
This file reads all the examples from the text files of the
/test/inputs/grammar_rules directory, compiles them and writes the compiled
version into identically named files in the /test/outputs/grammar_rules
directory
-}

{-# language LambdaCase, FlexibleInstances #-}

module Main where

-- imports

import Prelude qualified as P
import System.Environment qualified as SE
import Text.Parsec qualified as TP
import Data.List.Split qualified as SPL

import Helpers qualified as H
import ASTTypes qualified as T

import Parsing.AST qualified as PAST

import Generation.TypesAndHelpers qualified as TH
import Generation.AST qualified as GAST

-- types

type FileString = P.String
type TestExample = P.String

type CompileExFunc = TestExample -> P.String

type Compile a = H.Lcases -> ResultString a
newtype ResultString a = RS P.String

-- main

main :: P.IO ()
main = P.mapM_ compile_examples file_name_compile_func_pairs

-- compile the examples of a file

compile_examples :: (H.FileName, CompileExFunc) -> P.IO ()
compile_examples (file_name, comp_ex_func) =
  read_examples file_name H.>$> P.concatMap comp_ex_func P.>>= \ex_outs ->
  get_out_path P.>>= \out_path ->
  P.writeFile out_path ex_outs
  where
  get_out_path :: P.IO P.FilePath
  get_out_path = get_test_outputs_path H.>$> (P.++ H.make_extension_hs file_name)

get_test_outputs_path :: P.IO P.FilePath
get_test_outputs_path = SE.getArgs H.>$> (P.!!1) H.>$> (P.++ "/")

compile_example_func :: (PAST.HasParser a, TH.ToHaskell a) => Compile a
compile_example_func = PAST.parse H..> parse_res_to_final_res

parse_res_to_final_res ::
  TH.ToHaskell a => P.Either TP.ParseError a -> ResultString a
parse_res_to_final_res = RS P.. (P.++ "\n\n") P.. \case
  P.Left err -> "Error :( ==>" P.++ P.show err
  P.Right res -> TH.to_haskell res

extract_res_str :: ResultString a -> TH.Haskell
extract_res_str = \(RS s) -> s

-- Reading the examples from a file

read_examples :: H.FileName -> P.IO [FileString]
read_examples = \file_name -> read_exs_file file_name H.>$> file_str_to_examples

read_exs_file :: H.FileName -> P.IO FileString
read_exs_file = \file_name ->
  get_test_inputs_path H.>$> (P.++ file_name) P.>>= P.readFile

get_test_inputs_path :: P.IO P.FilePath
get_test_inputs_path = SE.getArgs H.>$> P.head H.>$> (P.++ "/")

file_str_to_examples :: FileString -> [ TestExample ]
file_str_to_examples = SPL.endBy "#\n\n"

-- Pairs of file names and the correcsponding compile function for
-- each example in the file

file_name_compile_func_pairs :: [(H.FileName, CompileExFunc)]
file_name_compile_func_pairs =
  [ ( "literals.txt"
    , (compile_example_func :: Compile (TH.NeedsAnnotBool, T.Literal)) H..>
      extract_res_str
    )
  , ( "identifiers.txt"
    , (compile_example_func :: Compile T.Identifier) H..> extract_res_str
    )
  , ( "paren_expr.txt"
    , (compile_example_func :: Compile T.ParenExpr) H..> extract_res_str
    )
  , ( "tuple.txt"
    , (compile_example_func :: Compile T.Tuple) H..> extract_res_str
    )
  , ( "big_tuple.txt"
    , (compile_example_func :: Compile (THWIL T.BigTuple)) H..> extract_res_str
    )
  , ( "list.txt"
    , (compile_example_func :: Compile T.List) H..> extract_res_str
    )
  , ( "big_list.txt"
    , (compile_example_func :: Compile (THWIL T.BigList)) H..> extract_res_str
    )
  , ( "paren_func_app.txt"
    , (compile_example_func :: Compile T.ParenFuncAppOrId) H..> extract_res_str
    )
  , ( "prefix_func_app.txt"
    , (compile_example_func :: Compile T.PreFuncApp) H..> extract_res_str
    )
  , ( "postfix_func_app.txt"
    , (compile_example_func :: Compile T.PostFuncApp) H..> extract_res_str
    )
  , ( "line_op_expr.txt"
    , (compile_example_func :: Compile T.LineOpExpr) H..> extract_res_str
    )
  , ( "big_op_expr.txt"
    , (compile_example_func :: Compile (THWIL T.BigOpExpr)) H..> extract_res_str
    )
  , ( "line_func_expr.txt"
    , (compile_example_func :: Compile T.LineFuncExpr) H..> extract_res_str
    )
  , ( "big_func_expr.txt"
    , ( compile_example_func ::
        Compile (THWIL (T.BigFuncExpr, TH.PossiblyWhereExpr))
      ) H..> extract_res_str
    )
  , ( "cases_func_expr.txt"
    , ( compile_example_func ::
        Compile (THWIL (T.CasesFuncExpr, TH.PossiblyWhereExpr))
      ) H..> extract_res_str
    )
  , ( "value_def.txt"
    , (compile_example_func :: Compile (THWIL T.ValueDef)) H..> extract_res_str
    )
  , ( "grouped_val_defs.txt"
    , (compile_example_func :: Compile (THWIL T.GroupedValueDefs)) H..>
      extract_res_str
    )
  , ( "where_expr.txt"
    , (compile_example_func :: Compile (THWIL T.WhereExpr)) H..> extract_res_str
    )
  , ( "type_id.txt"
    , (compile_example_func :: Compile T.TypeId) H..> extract_res_str
    )
  , ( "func_type.txt"
    , (compile_example_func :: Compile T.FuncType) H..> extract_res_str
    )
  , ( "prod_type.txt"
    , (compile_example_func :: Compile T.ProdType) H..> extract_res_str
    )
  , ( "type_app.txt"
    , (compile_example_func :: Compile (TH.NeedsParenBool, T.TypeAppIdOrAHTV))
      H..>
      extract_res_str
    )
  , ( "cond_type.txt"
    , (compile_example_func :: Compile T.Type) H..> extract_res_str
    )
  , ( "tuple_type_def.txt"
    , (compile_example_func :: Compile T.TupleTypeDef) H..> extract_res_str
    )
  , ( "or_type_def.txt"
    , (compile_example_func :: Compile T.OrTypeDef) H..> extract_res_str
    )
  , ( "type_nickname.txt"
    , (compile_example_func :: Compile T.TypeNickname) H..> extract_res_str
    )
  , ( "atom_prop_def.txt"
    , (compile_example_func :: Compile T.AtomPropDef) H..> extract_res_str
    )
  , ( "renaming_prop_def.txt"
    , (compile_example_func :: Compile T.RenamingPropDef) H..> extract_res_str
    )
  , ( "type_theo.txt"
    , (compile_example_func :: Compile T.TypeTheo) H..> extract_res_str
    )
  , ( "param_tv.txt"
    , (compile_example_func :: Compile T.ParamTVar) H..> extract_res_str
    )
  , ( "ad_hoc_tv.txt"
    , (compile_example_func :: Compile T.AdHocTVar) H..> extract_res_str
    )
  ]

-- to have TH.ToHaskell from TH.ToHsWithIndentLvl and also PAST.HasParser
-- for compile_example_func

newtype THWIL a = THWIL a

instance TH.ToHsWithIndentLvl a => TH.ToHaskell (THWIL a) where
   to_haskell (THWIL a) = TH.to_hs_wil a H.&> TH.run_generator

instance PAST.HasParser a => PAST.HasParser (THWIL a) where
   parser = THWIL P.<$> PAST.parser

instance PAST.HasParser a => PAST.HasParser (a, TH.PossiblyWhereExpr) where
   parser = PAST.parser H.>$> \a -> (a, TH.NoWhereExpr)

instance PAST.HasParser a => PAST.HasParser (TH.NeedsParenBool, a) where
   parser = PAST.parser H.>$> \a -> (TH.NoParen, a)

instance PAST.HasParser a => PAST.HasParser (TH.NeedsAnnotBool, a) where
   parser = PAST.parser H.>$> \a -> (TH.NoAnnot, a)

-- For fast vim file navigation:
{-
ASTTypes.hs
-}
