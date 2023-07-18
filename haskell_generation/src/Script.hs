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

correct_names = 
  fmap (lines .> map (only_name .> ("correct/" ++)) ) $
  readCreateProcess (shell "cd lcases/correct; ls") ""
  :: IO [ String ]

wrong_names = 
  fmap (lines .> map (only_name .> ("wrong/" ++)) ) $
  readCreateProcess (shell "cd lcases/wrong; ls") ""
  :: IO [ String ]

all_names =
  (++) <$> correct_names <*> wrong_names
  :: IO [ String ]

-- lcases: paths

paths =
  fmap (map ( \s -> "lcases/" ++ s ++ ".lc" )) all_names
  :: IO [ String ]

-- haskell: hs_dir, hs_paths, path_pairs, correct_hs_paths

hs_dir = "runtime/haskell/"
  :: String

hs_paths =
  fmap (map ( \s -> hs_dir ++ s ++ ".hs" )) all_names
  :: IO [ String ]

path_pairs = 
  liftA2 zip paths hs_paths
  :: IO [ (Path, Path) ]

correct_hs_paths =
  fmap (map ( \s -> hs_dir ++ s ++ ".hs" )) correct_names
  :: IO [ String ]

-- executables

execs_dir = "runtime/executables/"
  :: String

exec_paths = 
  fmap (map ( \s -> execs_dir ++ s )) correct_names
  :: IO [ String ]

exec_path_pairs = 
  liftA2 zip correct_hs_paths exec_paths
  :: IO [ (Path, Path) ]

-- commands 

exec_path_pair_to_cmd = ( \(hs_path, exec_path) -> 
  callCommand $ "ghc -o " ++ exec_path ++ " " ++ hs_path
  ) :: (Path, Path) -> IO ()

clean =
  callCommand ("rm " ++ hs_dir ++ "correct/*.hi " ++ hs_dir ++ "correct/*.o")
  :: IO ()

run_execs =
  callCommand ("for f in " ++ execs_dir ++ "correct/*; do echo \"\n$f\n\"; $f; done")
  :: IO ()

-- header

haskell_header =
  "runtime/header.hs"
  :: Path
