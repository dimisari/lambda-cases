{-# LANGUAGE LambdaCase, FlexibleInstances #-}

module Generation.Test where

import System.Process
import Data.List
import Text.Parsec (runParser, eof, ParseError)

import Helpers
import ASTTypes

import Parsing.AST

import Generation.TypesAndHelpers
import Generation.Collect
import Generation.Preprocess
import Generation.AST

-- types
type CompileExFunc = TestExample -> String

type Lcases = String

type Compile a = Lcases -> ResultString a
newtype ResultString a = RS String

-- paths
res_dir = "../hs_gen_results/"
  :: FilePath

-- main
main :: IO ()
main =
  list_progs >>= mapM_ compile_prog >> callCommand "./compile.sh" >>
  mapM_ compile_example file_name_compile_func_pairs

-- compile_prog
compile_prog :: ProgramFileName -> IO ()
compile_prog pfn =
  read_prog pfn >>= compile .> writeFile out_path
  where
  out_path :: FilePath
  out_path = res_dir ++ progs_dir ++ make_extension_hs pfn

compile :: Lcases -> String
compile =
  parse .> parse_res_to_final_res
  where
  parse_res_to_final_res :: Either ParseError Program -> String
  parse_res_to_final_res = \case
    Left err -> "Error :( ==>" ++ show err
    Right prog -> prog_to_hs prog

  prog_to_hs :: Program -> Haskell
  prog_to_hs = change_prog_if_needed .> to_haskell .> (top_hs ++)

  top_hs :: Haskell
  top_hs = lang_exts ++ imports

  lang_exts :: Haskell
  lang_exts = "{-# language " ++ intercalate ", " lang_ext_names ++ " #-}\n"

  lang_ext_names :: [Haskell]
  lang_ext_names =
    ["FlexibleInstances", "MultiParamTypeClasses", "ScopedTypeVariables"]

  imports :: Haskell
  imports = concatMap (\im_n -> "import " ++ im_n ++ "\n") import_names ++ "\n"

  import_names :: [Haskell]
  import_names =
    ["Prelude hiding (IO)", "Haskell.Predefined", "Haskell.OpsInHaskell"]

-- compile_example
compile_example :: (FileName, CompileExFunc) -> IO ()
compile_example (file_name, comp_ex_func) =
  read_examples file_name >>= concatMap comp_ex_func .> writeFile out_path
  where
  out_path :: FilePath
  out_path = res_dir ++ test_exs_dir ++ make_extension_hs file_name

compile_example_func :: (HasParser a, ToHaskell a) => Compile a
compile_example_func = parse .> parse_res_to_final_res

parse_res_to_final_res :: ToHaskell a => Either ParseError a -> ResultString a
parse_res_to_final_res = RS . (++ "\n\n") . \case
  Left err -> "Error :( ==>" ++ show err
  Right res -> to_haskell res

extract_res_str :: ResultString a -> Haskell
extract_res_str = \(RS s) -> s

-- file_name_compile_func_pairs
file_name_compile_func_pairs :: [(FileName, CompileExFunc)]
file_name_compile_func_pairs =
  [ ( "literals.txt"
    , (compile_example_func :: Compile Literal) .> extract_res_str
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

-- For fast vim navigation
-- TypesAndHelpers.hs
-- Collect.hs
-- Preprocess.hs
-- AST.hs
-- ASTTypes.hs
