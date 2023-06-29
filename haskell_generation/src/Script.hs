module Script where

import GHC.Base (liftA2)
import System.Process (callCommand, readCreateProcess, shell)

import Helpers ((.>))

-- All: Path, Constants, Types, Parsing, Generating Haskell, main

-- Path: Path
 
type Path = String 

-- names:
-- correct_names, wrong_names, all_names

-- hs_paths, path_pairs, 

-- in_out_path_pairs, haskell_header

only_name = ( \s -> take (length s - 3) s )
  :: String -> String

io_correct_names = 
  fmap (lines .> map (only_name .> ("correct/" ++)) ) $
  readCreateProcess (shell "cd lcases/correct; ls") ""
  :: IO [ String ]

io_wrong_names = 
  fmap (lines .> map (only_name .> ("wrong/" ++)) ) $
  readCreateProcess (shell "cd lcases/wrong; ls") ""
  :: IO [ String ]

io_all_names =
  (++) <$> io_correct_names <*> io_wrong_names
  :: IO [ String ]

-- lcases: paths

io_paths =
  fmap (map ( \s -> "lcases/" ++ s ++ ".lc" )) io_all_names
  :: IO [ String ]

-- haskell: hs_dir, hs_paths, path_pairs, correct_hs_paths

hs_dir = "runtime/haskell/"
  :: String

io_hs_paths =
  fmap (map ( \s -> hs_dir ++ s ++ ".hs" )) io_all_names
  :: IO [ String ]

io_path_pairs = 
  liftA2 zip io_paths io_hs_paths
  :: IO [ (Path, Path) ]

io_correct_hs_paths =
  fmap (map ( \s -> hs_dir ++ s ++ ".hs" )) io_correct_names
  :: IO [ String ]

-- executables

execs_dir = "runtime/executables/"
  :: String

io_exec_paths = 
  fmap (map ( \s -> execs_dir ++ s )) io_correct_names
  :: IO [ String ]

io_exec_path_pairs = 
  liftA2 zip io_correct_hs_paths io_exec_paths
  :: IO [ (Path, Path) ]

exec_path_pair_to_cmd = ( \(hs_path, exec_path) -> 
  callCommand $ "ghc -o " ++ exec_path ++ " " ++ hs_path
  ) :: (Path, Path) -> IO ()

-- header

haskell_header =
  "runtime/header.hs"
  :: Path
