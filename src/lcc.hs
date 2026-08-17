{-
This file contains the main function.
It receives the haskell translation of the program and adds imports and
language extensions.
It spits out haskell or an executable depending on the program arguments.
-}
{-# language LambdaCase #-}

module Main where

-- imports

import Prelude ((++), (>>), (>>=), ($), (<$>))
import Prelude qualified as P

import System.Environment qualified as SE
import System.Process qualified as SP
import Data.List qualified as DL
import Text.Parsec qualified as TP

import ASTTypes qualified as T
import Helpers ((.>), (>$>), (&>), (>++<))
import Helpers qualified as H

import Parsing.ASTInstances qualified as PA

import SyntaxTreeGen.TypesAndClasses qualified as STC
import SyntaxTreeGen.ASTInstances qualified as SA

import Preprocessing.Preprocess qualified as PP

import Generation.TypesAndClasses qualified as GTC
import Generation.ASTInstances qualified as GA

import System.Directory qualified as SD
import System.FilePath qualified as SFP

-- types

type ProgramFileName = H.FileName
type HsFileName = P.String
type ErrChoiceAndProgName = (ThrowErrorOrDont, ProgramFileName)
type ParseErrOrGenRes = P.Either TP.ParseError P.String
type GenerateFunction = T.Program -> P.String
type GenerateTuple = (GenerateFunction, ThrowErrorOrDont)
type CompileFunction = H.Lcases -> P.String
type CompileTuple = (CompileFunction, ProgramFileName, NewExtension)
type NewExtension = P.String

data ThrowErrorOrDont = Throw_err | Dont_throw_err

-- main

main :: P.IO ()
main = SE.getArgs >>= \case
  [] -> P.putStrLn "No arguments"
  [program_file_name] -> compile_and_run program_file_name
  ["-c", program_file_name] -> compile_for_tests program_file_name
  ["-h", program_file_name] -> compile_to_hs_or_error_file program_file_name
  ["-d", program_file_name] -> compile_file_to_dot program_file_name
  ["-p", program_file_name] -> compile_file_to_png program_file_name
  _  -> P.putStrLn "Weird arguments"

-- compiling files

compile_and_run :: ProgramFileName -> P.IO ()
compile_and_run = \pfn ->
  compile_to_exec_gen (Throw_err, pfn) >>
  P.putStrLn "\nRunning\n" >>
  SP.callCommand ("./" ++ SFP.dropExtension pfn)

compile_for_tests :: ProgramFileName -> P.IO ()
compile_for_tests = (Dont_throw_err,) .> compile_to_exec_gen

compile_to_exec_gen :: ErrChoiceAndProgName -> P.IO ()
compile_to_exec_gen = \ecapn ->
  get_ghc_command >>= \ghc_command ->
  ecapn_to_hs_file ecapn >>= \hs_file ->
  run_ghc_and_remove_hs_file ghc_command hs_file

run_ghc_and_remove_hs_file :: P.String -> HsFileName -> P.IO ()
run_ghc_and_remove_hs_file = \ghc_command hs_file ->
  SP.callCommand (ghc_command ++ hs_file ++ " && rm " ++ hs_file)

get_ghc_command :: P.IO P.String
get_ghc_command =
   ("ghc" ++) <$> get_predef_imports >$>
   (++ " -no-keep-hi-files -no-keep-o-files ")

compile_to_hs_or_error_file :: ProgramFileName -> P.IO ()
compile_to_hs_or_error_file =
  (Dont_throw_err,) .> ecapn_to_hs_file .> (>> P.pure ())

ecapn_to_hs_file :: ErrChoiceAndProgName -> P.IO HsFileName
ecapn_to_hs_file ecapn@(teon, pfn) =
  ecapn_to_hs ecapn >>= \comp_hs ->
  get_lang_exts_and_imports_hs >>= \lang_exts_and_imports_hs ->
  P.writeFile hs_file (lang_exts_and_imports_hs ++ comp_hs) >>
  P.pure hs_file
  where
  hs_file :: HsFileName
    = H.make_extension "hs" pfn

ecapn_to_hs :: ErrChoiceAndProgName -> P.IO GTC.Haskell
ecapn_to_hs (teon, pfn) = compile_file_to_string (compile_lc_to_hs teon) pfn

compile_file_to_png :: ProgramFileName -> P.IO ()
compile_file_to_png = \pfn ->
  compile_file_to_dot pfn >> run_dot_and_remove_dot_file pfn

run_dot_and_remove_dot_file :: ProgramFileName -> P.IO ()
run_dot_and_remove_dot_file pfn =
  SP.callCommand
  ( "dot -T png " ++ dot_file ++ " > " ++ H.make_extension "png" pfn ++
    " && rm " ++ dot_file
  )
  where
  dot_file :: P.String
    = H.make_extension "dot" pfn

compile_file_to_dot :: ProgramFileName -> P.IO ()
compile_file_to_dot = \pfn ->
  compile_file_to_file (compile_lc_to_dot, pfn, "dot")

compile_file_to_file :: CompileTuple -> P.IO ()
compile_file_to_file = \(cf, pfn, ne) ->
  compile_file_to_string cf pfn >>= P.writeFile (H.make_extension ne pfn)

compile_file_to_string :: CompileFunction -> ProgramFileName -> P.IO P.String
compile_file_to_string = \cf pfn -> read_prog_file pfn >$> cf

read_prog_file :: ProgramFileName -> P.IO H.Lcases
read_prog_file = H.add_dotlc_if_needed .> P.readFile

-- compiling to haskell

compile_lc_to_hs :: ThrowErrorOrDont -> H.Lcases -> GTC.Haskell
compile_lc_to_hs = \teon -> generate_to_compile (prog_to_hs, teon)

prog_to_hs :: T.Program -> GTC.Haskell
prog_to_hs = PP.preprocess_prog .> GTC.to_haskell

-- compiling to dot

compile_lc_to_dot :: H.Lcases -> STC.Dot
compile_lc_to_dot = generate_to_compile (SA.to_dot_final, Throw_err)

-- generate to compile

generate_to_compile :: GenerateTuple -> H.Lcases -> P.String
generate_to_compile = \(gen_f, teon) ->
  generate_to_compile_parse_err gen_f .> \case
    P.Left err -> throw_error_or_dont teon err
    P.Right a -> a

generate_to_compile_parse_err :: GenerateFunction -> H.Lcases -> ParseErrOrGenRes
generate_to_compile_parse_err = \gen -> PA.parse .> P.fmap gen

throw_error_or_dont :: ThrowErrorOrDont -> TP.ParseError -> P.String
throw_error_or_dont = \case
  Throw_err -> error_to_str .> P.error
  Dont_throw_err -> error_to_str

error_to_str :: TP.ParseError -> P.String
error_to_str = P.show .> ("Error :( ==> " ++)

-- language extensions and imports haskell

get_lang_exts_and_imports_hs :: P.IO GTC.Haskell
get_lang_exts_and_imports_hs = (lang_exts ++) <$> get_imports_hs

-- language extesions code

lang_ext_names :: [GTC.Haskell]
lang_ext_names =
  [ "FlexibleInstances", "MultiParamTypeClasses", "ScopedTypeVariables"
  , "UndecidableInstances", "FlexibleContexts"
  ]

lang_exts :: GTC.Haskell
lang_exts = "{-# language " ++ DL.intercalate ", " lang_ext_names ++ " #-}\n"

-- imports code

get_imports_hs :: P.IO GTC.Haskell
get_imports_hs = module_names_to_import_code <$> get_module_names

module_names_to_import_code :: [GTC.Haskell] -> GTC.Haskell
module_names_to_import_code = \module_names ->
  P.concatMap (\im_n -> "import " ++ im_n ++ "\n") module_names ++ "\n"

get_predef_imports :: P.IO P.String
get_predef_imports = get_predef_file_paths >$> P.concatMap (" --make " ++)

get_predef_dir :: P.IO P.FilePath
get_predef_dir = SE.getEnv "HOME" >$> (++ "/.local/share/lcc/Predefined/")

get_predef_files :: P.IO [P.String]
get_predef_files = get_predef_dir >>= SD.listDirectory

get_predef_file_paths :: P.IO [P.FilePath]
get_predef_file_paths =
  get_predef_dir >>= \dir -> (P.map (dir ++)) <$> get_predef_files

get_module_names :: P.IO [GTC.Haskell]
get_module_names = (["qualified Prelude as P"] ++) <$> get_predef_module_names

get_predef_module_names :: P.IO [GTC.Haskell]
get_predef_module_names = P.map file_to_module_name <$> get_predef_files

file_to_module_name :: GTC.Haskell -> GTC.Haskell
file_to_module_name = SFP.dropExtension .> ("Predefined." ++)
