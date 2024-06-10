{-# language LambdaCase #-}

module Main where

import System.Environment (getArgs)
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
main = getArgs >>= mapM_ compile_prog

-- compile_prog
compile_prog :: ProgramFileName -> IO ()
compile_prog pfn =
  readFile pfn >>= compile .> writeFile (make_extension_hs pfn)

compile :: Lcases -> String
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
  ["Prelude hiding (IO)", "PredefImports.Predefined", "PredefImports.OpsInHaskell"]

-- ASTTypes.hs
