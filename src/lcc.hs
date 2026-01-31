{-
This file contains the main function.
It receives the haskell translation of the program and adds imports and
language extensions.
It spits out haskell or an executable depending on the program arguments.
-}
{-# language LambdaCase #-}

module Main where

-- imports

import Prelude ((++), (>>), (>>=))
import Prelude qualified as P

import System.Environment qualified as SE
import System.Process qualified as SP
import Data.List qualified as DL
import Text.Parsec qualified as TP

import ASTTypes qualified as T
import Helpers ((.>), (>$>))
import Helpers qualified as H

import Parsing.AST qualified as PA

import Preprocessing.Preprocess qualified as GP
import Generation.TypesAndHelpers qualified as GTH
import Generation.AST qualified as GA

-- types

type ProgramFileName = H.FileName
type HsFileName = P.String

-- lists

predef_list :: P.IO [P.FilePath]
predef_list =
  SE.getEnv "HOME" >$> (++ "/.local/share/lcc/PredefImports/") >$> \x ->
    P.map (x ++) [ "OpsInHaskell.hs", "Predefined.hs" ]

lang_ext_names :: [GTH.Haskell]
lang_ext_names =
  [ "FlexibleInstances", "MultiParamTypeClasses", "ScopedTypeVariables"
  , "UndecidableInstances", "FlexibleContexts"
  ]

import_names :: [GTH.Haskell]
import_names =
  [ "qualified Prelude as P", "PredefImports.Predefined"
  , "PredefImports.OpsInHaskell"
  ]

-- main

main :: P.IO ()
main = SE.getArgs >>= \case
  [] -> P.putStrLn "No arguments"
  [program_file_name] -> compile_to_exec program_file_name
  ["-h", program_file_name] -> compile_to_hs program_file_name >> P.return ()
  _  -> P.putStrLn "Weird arguments"

compile_to_exec :: ProgramFileName -> P.IO ()
compile_to_exec pfn =
  compile_to_hs pfn >>= \hs_file ->
  ghc_compile >>= \ghc_compile_bash ->
  SP.callCommand (ghc_compile_bash ++ hs_file) >>
  SP.callCommand ("rm " ++ hs_file)
  where
  ghc_compile :: P.IO P.String
  ghc_compile =
    make_predef >$> ("ghc" ++) >$> (++ "-no-keep-hi-files -no-keep-o-files ")

  make_predef :: P.IO P.String
  make_predef = predef_list >$> P.concatMap (" --make " ++) >$> (++ " ")

compile_to_hs :: ProgramFileName -> P.IO HsFileName
compile_to_hs pfn =
  P.readFile pfn >>= compile .> P.writeFile hs_file >> P.return hs_file
  where
  hs_file :: P.String
  hs_file = H.make_extension_hs pfn

compile :: H.Lcases -> GTH.Haskell
compile =
  PA.parse .> parse_res_to_final_res
  where
  parse_res_to_final_res :: P.Either TP.ParseError T.Program -> P.String
  parse_res_to_final_res = \case
    P.Left err -> "Error :( ==> " ++ P.show err
    P.Right prog -> prog_to_hs prog

  prog_to_hs :: T.Program -> GTH.Haskell
  prog_to_hs = GP.preprocess_prog .> GTH.to_haskell .> (top_hs ++)

-- language extensions and imports

top_hs :: GTH.Haskell
top_hs = lang_exts ++ imports

lang_exts :: GTH.Haskell
lang_exts = "{-# language " ++ DL.intercalate ", " lang_ext_names ++ " #-}\n"

imports :: GTH.Haskell
imports = P.concatMap (\im_n -> "import " ++ im_n ++ "\n") import_names ++ "\n"
