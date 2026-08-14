{-
This file contains the main function.
It receives the haskell translation of the program and adds imports and
language extensions.
It spits out haskell or an executable depending on the program arguments.
-}
{-# language LambdaCase #-}

module Main where

-- imports

import Prelude ((++), (>>), (>>=), ($))
import Prelude qualified as P

import System.Environment qualified as SE
import System.Process qualified as SP
import Data.List qualified as DL
import Text.Parsec qualified as TP

import ASTTypes qualified as T
import Helpers ((.>), (>$>), (&>))
import Helpers qualified as H

import Parsing.AST qualified as PA

import Preprocessing.Preprocess qualified as GP
import Generation.TypesAndClasses qualified as GTC
import Generation.AST qualified as GA

import System.FilePath qualified as SFP

-- types

type ProgramFileName = H.FileName
type HsFileName = P.String
type GenerateFunction = P.Either TP.ParseError T.Program -> P.String
type CompileFunction = H.Lcases -> GTC.Haskell

-- main

main :: P.IO ()
main = SE.getArgs >>= \case
  [] -> P.putStrLn "No arguments"
  [program_file_name] -> compile_and_run program_file_name
  ["-c", program_file_name] -> compile_for_tests program_file_name
  ["-h", program_file_name] -> compile_to_hs_or_error_file program_file_name
  _  -> P.putStrLn "Weird arguments"

compile_and_run :: ProgramFileName -> P.IO ()
compile_and_run = \pfn ->
  compile_to_exec_gen compile_lc_to_hs pfn >> P.putStrLn "\nRunning\n" >>
  SP.callCommand ("./" ++ SFP.dropExtension pfn)

compile_for_tests :: ProgramFileName -> P.IO ()
compile_for_tests = compile_to_exec_gen compile_lc_to_hs_or_err_str

compile_to_exec_gen :: CompileFunction -> ProgramFileName -> P.IO ()
compile_to_exec_gen cf pfn =
  compile_to_and_get_file_gen cf pfn >>= \hs_file ->
  ghc_command >>= \ghc_cmd ->
  SP.callCommand (ghc_cmd ++ hs_file ++ " && " ++ "rm " ++ hs_file)

ghc_command :: P.IO P.String
ghc_command =
  make_predef >$> ("ghc" ++) >$> (++ " -no-keep-hi-files -no-keep-o-files ")

compile_to_hs_or_err_and_get_file :: ProgramFileName -> P.IO HsFileName
compile_to_hs_or_err_and_get_file =
  compile_to_and_get_file_gen compile_lc_to_hs_or_err_str

compile_to_and_get_file_gen
  :: CompileFunction -> ProgramFileName -> P.IO HsFileName
compile_to_and_get_file_gen = \cf pfn ->
  compile_to_file_gen cf pfn >> P.return (H.make_extension_hs pfn)

compile_to_hs_or_error_file :: ProgramFileName -> P.IO ()
compile_to_hs_or_error_file = compile_to_file_gen compile_lc_to_hs_or_err_str

compile_to_file_gen :: CompileFunction -> ProgramFileName -> P.IO ()
compile_to_file_gen = \cf pfn ->
  compile_file_to_hs_gen cf pfn >>= P.writeFile (H.make_extension_hs pfn)

compile_file_to_hs_gen :: CompileFunction -> ProgramFileName -> P.IO GTC.Haskell
compile_file_to_hs_gen = \cf pfn ->
  H.add_dotlc_if_needed pfn &> P.readFile >$> cf

compile_lc_to_hs_or_err_str :: CompileFunction
compile_lc_to_hs_or_err_str = compile generate_hs_or_error_str

compile_lc_to_hs :: CompileFunction
compile_lc_to_hs = compile generate_hs

compile :: GenerateFunction -> CompileFunction
compile = \gn -> PA.parse .> gn

generate_hs_or_error_str :: GenerateFunction
generate_hs_or_error_str = \case
  P.Left err -> error_to_str err
  P.Right prog -> prog_to_hs prog

generate_hs :: GenerateFunction
generate_hs = \case
  P.Left err -> P.error $ error_to_str err
  P.Right prog -> prog_to_hs prog

error_to_str :: TP.ParseError -> P.String
error_to_str = P.show .> ("Error :( ==> " ++)

prog_to_hs :: T.Program -> GTC.Haskell
prog_to_hs = GP.preprocess_prog .> GTC.to_haskell .> (top_hs ++)

-- language extensions and imports

make_predef :: P.IO P.String
make_predef = predef_list >$> P.concatMap (" --make " ++)

predef_list :: P.IO [P.FilePath]
predef_list =
  SE.getEnv "HOME" >$> (++ "/.local/share/lcc/Predefined/") >$> \x ->
    P.map (x ++) [ "Operators.hs", "Predefined.hs" ]

lang_ext_names :: [GTC.Haskell]
lang_ext_names =
  [ "FlexibleInstances", "MultiParamTypeClasses", "ScopedTypeVariables"
  , "UndecidableInstances", "FlexibleContexts"
  ]

import_names :: [GTC.Haskell]
import_names =
  [ "qualified Prelude as P", "Predefined.Predefined"
  , "Predefined.Operators"
  ]

top_hs :: GTC.Haskell
top_hs = lang_exts ++ imports

lang_exts :: GTC.Haskell
lang_exts = "{-# language " ++ DL.intercalate ", " lang_ext_names ++ " #-}\n"

imports :: GTC.Haskell
imports = P.concatMap (\im_n -> "import " ++ im_n ++ "\n") import_names ++ "\n"
