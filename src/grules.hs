{-
This file reads all the examples from the text files of the
/test/inputs/grammar_rules directory, compiles them and writes the compiled
version into identically named files in the /test/outputs/grammar_rules
directory
-}

{-# language LambdaCase, FlexibleInstances #-}

module Main where

-- imports

import Prelude ((>>=), (++), (!!), (.), (<$>))
import Prelude qualified as P
import System.Environment qualified as SE
import Text.Parsec qualified as TP
import Data.List.Split qualified as SPL

import Helpers ((>$>), (.>), (&>))
import Helpers qualified as H
import ASTTypes qualified as T

import Parsing.AST qualified as PA

import Generation.TypesAndHelpers qualified as GTH
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
  read_examples file_name >$> P.concatMap comp_ex_func >>= \ex_outs ->
  get_out_path >>= \out_path ->
  P.writeFile out_path ex_outs
  where
  get_out_path :: P.IO P.FilePath
  get_out_path = get_test_outputs_path >$> (++ H.make_extension_hs file_name)

get_test_outputs_path :: P.IO P.FilePath
get_test_outputs_path = SE.getArgs >$> (!!1) >$> (++ "/")

compile_example_func :: (PA.HasParser a, GTH.ToHaskell a) => Compile a
compile_example_func = PA.parse .> parse_res_to_final_res

parse_res_to_final_res ::
  GTH.ToHaskell a => P.Either TP.ParseError a -> ResultString a
parse_res_to_final_res = RS . (++ "\n\n") . \case
  P.Left err -> "Error :( ==>" ++ P.show err
  P.Right res -> GTH.to_haskell res

extract_res_str :: ResultString a -> GTH.Haskell
extract_res_str = \(RS s) -> s

-- Reading the examples from a file

read_examples :: H.FileName -> P.IO [FileString]
read_examples = \file_name -> read_exs_file file_name >$> file_str_to_examples

read_exs_file :: H.FileName -> P.IO FileString
read_exs_file = \file_name ->
  get_test_inputs_path >$> (++ file_name) >>= P.readFile

get_test_inputs_path :: P.IO P.FilePath
get_test_inputs_path = SE.getArgs >$> P.head >$> (++ "/")

file_str_to_examples :: FileString -> [ TestExample ]
file_str_to_examples = SPL.endBy "#\n\n"

-- Pairs of file names and the correcsponding compile function for
-- each example in the file

file_name_compile_func_pairs :: [(H.FileName, CompileExFunc)]
file_name_compile_func_pairs =
  [ ( "literals.txt"
    , (compile_example_func :: Compile (GTH.NeedsAnnotBool, T.Literal)) .>
      extract_res_str
    )
  , ( "identifiers.txt"
    , (compile_example_func :: Compile T.Identifier) .> extract_res_str
    )
  , ( "paren_expr.txt"
    , (compile_example_func :: Compile T.ParenExpr) .> extract_res_str
    )
  , ( "tuple.txt"
    , (compile_example_func :: Compile T.Tuple) .> extract_res_str
    )
  , ( "big_tuple.txt"
    , (compile_example_func :: Compile (THWIL T.BigTuple)) .> extract_res_str
    )
  , ( "list.txt"
    , (compile_example_func :: Compile T.List) .> extract_res_str
    )
  , ( "big_list.txt"
    , (compile_example_func :: Compile (THWIL T.BigList)) .> extract_res_str
    )
  , ( "paren_func_app.txt"
    , (compile_example_func :: Compile T.ParenFuncAppOrId) .> extract_res_str
    )
  , ( "prefix_func_app.txt"
    , (compile_example_func :: Compile T.PreFuncApp) .> extract_res_str
    )
  , ( "postfix_func_app.txt"
    , (compile_example_func :: Compile T.PostFuncApp) .> extract_res_str
    )
  , ( "line_op_expr.txt"
    , (compile_example_func :: Compile T.LineOpExpr) .> extract_res_str
    )
  , ( "big_op_expr.txt"
    , (compile_example_func :: Compile (THWIL T.BigOpExpr)) .> extract_res_str
    )
  , ( "line_func_expr.txt"
    , (compile_example_func :: Compile T.LineFuncExpr) .> extract_res_str
    )
  , ( "big_func_expr.txt"
    , ( compile_example_func ::
        Compile (THWIL (T.BigFuncExpr, GTH.PossiblyWhereExpr))
      ) .> extract_res_str
    )
  , ( "cases_func_expr.txt"
    , ( compile_example_func ::
        Compile (THWIL (T.CasesFuncExpr, GTH.PossiblyWhereExpr))
      ) .> extract_res_str
    )
  , ( "value_def.txt"
    , (compile_example_func :: Compile (THWIL T.ValueDef)) .> extract_res_str
    )
  , ( "grouped_val_defs.txt"
    , (compile_example_func :: Compile (THWIL T.GroupedValueDefs)) .>
      extract_res_str
    )
  , ( "where_expr.txt"
    , (compile_example_func :: Compile (THWIL T.WhereExpr)) .> extract_res_str
    )
  , ( "type_id.txt"
    , (compile_example_func :: Compile T.TypeId) .> extract_res_str
    )
  , ( "func_type.txt"
    , (compile_example_func :: Compile T.FuncType) .> extract_res_str
    )
  , ( "prod_type.txt"
    , (compile_example_func :: Compile T.ProdType) .> extract_res_str
    )
  , ( "type_app.txt"
    , (compile_example_func :: Compile (GTH.NeedsParenBool, T.TypeAppIdOrAHTV))
      .>
      extract_res_str
    )
  , ( "cond_type.txt"
    , (compile_example_func :: Compile T.Type) .> extract_res_str
    )
  , ( "tuple_type_def.txt"
    , (compile_example_func :: Compile T.TupleTypeDef) .> extract_res_str
    )
  , ( "or_type_def.txt"
    , (compile_example_func :: Compile T.OrTypeDef) .> extract_res_str
    )
  , ( "type_nickname.txt"
    , (compile_example_func :: Compile T.TypeNickname) .> extract_res_str
    )
  , ( "atom_prop_def.txt"
    , (compile_example_func :: Compile T.AtomPropDef) .> extract_res_str
    )
  , ( "renaming_prop_def.txt"
    , (compile_example_func :: Compile T.RenamingPropDef) .> extract_res_str
    )
  , ( "type_theo.txt"
    , (compile_example_func :: Compile T.TypeTheo) .> extract_res_str
    )
  , ( "param_tv.txt"
    , (compile_example_func :: Compile T.ParamTVar) .> extract_res_str
    )
  , ( "ad_hoc_tv.txt"
    , (compile_example_func :: Compile T.AdHocTVar) .> extract_res_str
    )
  ]

-- to have GTH.ToHaskell from GTH.ToHsWithIndentLvl and also PA.HasParser
-- for compile_example_func

newtype THWIL a = THWIL a

instance GTH.ToHsWithIndentLvl a => GTH.ToHaskell (THWIL a) where
   to_haskell (THWIL a) = GTH.to_hs_wil a &> GTH.run_generator

instance PA.HasParser a => PA.HasParser (THWIL a) where
   parser = THWIL <$> PA.parser

instance PA.HasParser a => PA.HasParser (a, GTH.PossiblyWhereExpr) where
   parser = PA.parser >$> \a -> (a, GTH.NoWhereExpr)

instance PA.HasParser a => PA.HasParser (GTH.NeedsParenBool, a) where
   parser = PA.parser >$> \a -> (GTH.NoParen, a)

instance PA.HasParser a => PA.HasParser (GTH.NeedsAnnotBool, a) where
   parser = PA.parser >$> \a -> (GTH.NoAnnot, a)

-- For fast vim file navigation:
{-
ASTTypes.hs
-}
