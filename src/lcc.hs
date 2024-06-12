{-# language LambdaCase #-}

module Main where

import System.Environment (getArgs)
import System.Process (callCommand)
import Data.List (intercalate)
import Text.Parsec (ParseError)

import ASTTypes
import Helpers

import Parsing.AST (parse)

import Generation.Preprocess (preprocess_prog)
import Generation.TypesAndHelpers
import Generation.AST


-- main
main :: IO ()
main = getArgs >>= \case
  [] -> putStrLn "No arguments"
  [program_file_name] ->  compile_to_exec program_file_name
  ["-h", program_file_name] -> compile_to_hs program_file_name >> return ()
  _  -> putStrLn "Weird arguments"

compile_to_exec :: ProgramFileName -> IO ()
compile_to_exec pfn =
  compile_to_hs pfn >>= \hs_file ->
  callCommand (ghc_compile ++ hs_file) >>
  callCommand ("rm " ++ hs_file)
  where
  ghc_compile :: String
  ghc_compile = "ghc -no-keep-hi-files -no-keep-o-files "

compile_to_hs :: ProgramFileName -> IO HsFileName
compile_to_hs pfn =
  readFile pfn >>= compile .> writeFile hs_file >> return hs_file
  where
  hs_file :: String
  hs_file = make_extension_hs pfn

compile :: Lcases -> Haskell
compile =
  parse .> parse_res_to_final_res
  where
  parse_res_to_final_res :: Either ParseError Program -> String
  parse_res_to_final_res = \case
    Left err -> "Error :( ==>" ++ show err
    Right prog -> prog_to_hs prog

  prog_to_hs :: Program -> Haskell
  prog_to_hs = preprocess_prog .> to_haskell .> (top_hs ++)

-- language extensions and imports
top_hs :: Haskell
top_hs = lang_exts ++ imports

lang_exts :: Haskell
lang_exts = "{-# language " ++ intercalate ", " lang_ext_names ++ " #-}\n"

lang_ext_names :: [Haskell]
lang_ext_names =
  [ "FlexibleInstances", "MultiParamTypeClasses", "ScopedTypeVariables"
  , "UndecidableInstances"
  ]

imports :: Haskell
imports = concatMap (\im_n -> "import " ++ im_n ++ "\n") import_names ++ "\n"

import_names :: [Haskell]
import_names =
  [ "Prelude hiding (IO)", "PredefImports.Predefined"
  , "PredefImports.OpsInHaskell"
  ]

-- ASTTypes.hs
