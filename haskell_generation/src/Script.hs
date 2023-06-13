module Script where

import System.Process (callCommand)

-- All: Path, Constants, Types, Parsing, Generating Haskell, main

-- Path: Path
 
type Path = String 

-- names:
-- correct_names, wrong_names, all_names

-- hs_paths, path_pairs, 

-- in_out_path_pairs, haskell_header

correct_names =
  map ("correct/" ++)
    [
    "my_gcd"
    , "ext_euc_no_tuple_type"
    , "ext_euc_tuple_type"
    , "pair"
    , "bool"
    , "possibly_int"
    , "int_list"
    , "int_list2"
    , "int_list3"
    , "hanoi"
    ]
  :: [ String ]

wrong_names =
  map ("wrong/" ++)
  [
  "bool"
  , "not_covered"
  , "duplicate"
  , "out_of_scope"
  , "out_of_scope2"
  , "out_of_scope3"
  , "out_of_scope4"
  , "or_t_use_fields"
  , "func_t_use_fields"
  , "not_func"
  , "not_func2"
  , "not_func3"
  , "type_check_err"
  , "equ_err"
  , "add_err"
  , "dup_int_case"
  , "out_of_scope5"
  , "wrong_int_case"
  , "int_str"
  ]
  :: [ String ]

all_names =
  correct_names ++ wrong_names
  :: [ String ]

-- lcases: paths

paths =
  map ( \s -> "lcases/" ++ s ++ ".lc" ) all_names
  :: [ String ]

-- haskell: hs_dir, hs_paths, path_pairs, correct_hs_paths

hs_dir = "runtime/haskell/"
  :: String

hs_paths =
  map ( \s -> hs_dir ++ s ++ ".hs" ) all_names
  :: [ String ]

path_pairs = 
  zip paths hs_paths
  :: [ (Path, Path) ]

correct_hs_paths =
  map ( \s -> hs_dir ++ s ++ ".hs" ) correct_names
  :: [ String ]

-- executables

execs_dir = "runtime/executables/"
  :: String

exec_paths = 
  map ( \s -> execs_dir ++ s ) correct_names
  :: [ String ]

exec_path_pairs = 
  zip correct_hs_paths exec_paths
  :: [ (Path, Path) ]

exec_path_pair_to_cmd = ( \(hs_path, exec_path) -> 
  callCommand $ "ghc -o " ++ exec_path ++ " " ++ hs_path
  ) :: (Path, Path) -> IO ()

-- header

haskell_header =
  "runtime/header.hs"
  :: Path
