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
import Data.Set qualified as S
import Data.Map qualified as M
import Text.Parsec qualified as TP

import System.Directory qualified as SD
import System.FilePath qualified as SFP

import Control.Monad qualified as CM

import ASTTypes qualified as T
import Helpers ((.>), (>$>), (&>), (>++<))
import Helpers qualified as H

import Parsing.Helpers qualified as PH

import SyntaxTreeGen.TypesAndClasses qualified as STC
import SyntaxTreeGen.Helpers qualified as SH
import SyntaxTreeGen.ToStringTreeInstances qualified as STSTI

import Preprocessing.TypesAndClasses qualified as PTC
import Preprocessing.Preprocess qualified as PP
import Preprocessing.Collect qualified as PC

import Generation.TypesAndClasses qualified as GTC
import Generation.Instances qualified as GA

-- types

type ProgramPath = H.FilePath
type HsFilePath = P.String
type ErrChoiceAndProgPath = (ThrowErrorOrDont, ProgramPath)
type CompileToHsTuple = (ThrowErrorOrDont, ProgramPath, HsFilePath)
type ParseErrOrGenRes = P.Either TP.ParseError P.String
type GenerateFunction = T.Program -> P.String
type GenerateTuple = (GenerateFunction, ThrowErrorOrDont)
type CompileFunction = H.Lcases -> P.String
type NewExtension = P.String

data ThrowErrorOrDont = Throw_err | Dont_throw_err

-- main

main :: P.IO ()
main = SE.getArgs >>= \case
  [] -> P.putStrLn "No arguments"
  [program_path] -> compile_and_run program_path
  ["-c", program_path] -> compile_for_tests program_path
  ["-h", program_path] -> compile_to_hs_or_error_file program_path
  ["-d", program_path] -> compile_file_to_dot_short program_path
  ["-df", program_path] -> compile_file_to_dot_full program_path
  ["-pdf", program_path] -> compile_file_to_prep_dot_full program_path
  ["-s", program_path] -> compile_file_to_svg_short program_path
  ["-sf", program_path] -> compile_file_to_svg_full program_path
  ["-psf", program_path] -> compile_file_to_prep_svg_full program_path
  _  -> P.putStrLn "Weird arguments"

-- compiling files

compile_and_run :: ProgramPath -> P.IO ()
compile_and_run = \pp ->
  compile_to_exec (Throw_err, pp) >>
  P.putStrLn "\nRunning\n" >>
  SP.callCommand ("./" ++ SFP.dropExtension pp)

compile_for_tests :: ProgramPath -> P.IO ()
compile_for_tests = (Dont_throw_err,) .> compile_to_exec

compile_to_exec :: ErrChoiceAndProgPath -> P.IO ()
compile_to_exec (teod, pp) =
  get_import_lines pp >>= \import_lines ->
  get_ghc_command pp import_lines >>= \ghc_command ->
  compile_import_lines pp import_lines >>= \mf_ovis ->
  compile_to_hs_file_with_ovis mf_ovis teod pp >>
  run_ghc_and_remove_hs_file ghc_command hs_file >>
  remove_import_lines_hs_files pp import_lines
  where
  hs_file :: HsFilePath
    = H.make_extension "hs" pp

get_ghc_command :: ProgramPath -> [T.ImportLine] -> P.IO P.String
get_ghc_command = \pp ils ->
  ("ghc" ++) <$> get_predef_imports >$>
  (++ P.concatMap (" --make " ++) (P.map (pp_il_to_hs_file pp) ils)) >$>
  (++ " -no-keep-hi-files -no-keep-o-files ")

pp_il_to_hs_file :: ProgramPath -> T.ImportLine -> P.String
pp_il_to_hs_file = \pp il ->
  H.import_hs_file pp (import_line_to_import_prefix il)

import_line_to_import_prefix :: T.ImportLine -> P.String
import_line_to_import_prefix = \(T.ImL (_, T.IP ip)) -> ip

get_import_lines :: ProgramPath -> P.IO [T.ImportLine]
get_import_lines = read_prog_file .> P.fmap source_to_import_lines

run_ghc_and_remove_hs_file :: P.String -> HsFilePath -> P.IO ()
run_ghc_and_remove_hs_file = \ghc_command hs_file ->
  SP.callCommand (ghc_command ++ hs_file ++ " && rm " ++ hs_file)

remove_import_line_hs_file :: ProgramPath -> T.ImportLine -> P.IO ()
remove_import_line_hs_file = \pp il ->
  SP.callCommand ("rm " ++ pp_il_to_hs_file pp il)

remove_import_lines_hs_files :: ProgramPath -> [T.ImportLine] -> P.IO ()
remove_import_lines_hs_files = \pp -> P.mapM_ (remove_import_line_hs_file pp)

compile_to_hs_or_error_file :: ProgramPath -> P.IO ()
compile_to_hs_or_error_file = compile_to_hs_file Dont_throw_err

compile_to_hs_file_with_ovis
  :: PTC.MFOrValueIds -> ThrowErrorOrDont -> ProgramPath -> P.IO ()
compile_to_hs_file_with_ovis = \mf_ovis teod pp ->
  compile_to_hs_string_with_ovis mf_ovis teod pp >>=
  compile_to_hs_file_from_hs_string pp

compile_to_hs_string_with_ovis
  :: PTC.MFOrValueIds -> ThrowErrorOrDont -> ProgramPath -> P.IO GTC.Haskell
compile_to_hs_string_with_ovis = \mf_ovis teod pp ->
  compile_file_to_string (compile_lc_to_hs_with_ovis mf_ovis teod) pp

compile_to_hs_file :: ThrowErrorOrDont -> ProgramPath -> P.IO ()
compile_to_hs_file = \teod pp ->
  compile_file_to_string (compile_lc_to_hs teod) pp >>=
  compile_to_hs_file_from_hs_string pp

compile_to_hs_file_from_hs_string :: ProgramPath -> GTC.Haskell -> P.IO ()
compile_to_hs_file_from_hs_string = \pp hs ->
  get_lang_exts_and_imports_hs >>= \exts_imps_hs ->
  P.writeFile (H.make_extension "hs" pp) (exts_imps_hs ++ hs)

compile_import_lines :: ProgramPath -> [T.ImportLine] -> P.IO PTC.MFOrValueIds
compile_import_lines = \pp -> CM.foldM (compile_import_line pp) empty_mf_ovis

compile_import_line
  :: ProgramPath -> PTC.MFOrValueIds -> T.ImportLine -> P.IO PTC.MFOrValueIds
compile_import_line = \pp total_mf_ovis (T.ImL (T.IF imf, ip@(T.IP imp))) ->
  ip_pp_to_hs_and_or_values ip (H.import_lc_file pp imf) >>=
    \(comp_hs, new_mf_ovis) ->
  get_imports_hs >>= \imps_hs ->
  P.writeFile
    (H.import_hs_file pp imp)
    (lang_exts ++ H.module_line_hs imp ++ imps_hs ++ comp_hs) >>
  P.pure (PP.mf_ovis_union total_mf_ovis new_mf_ovis)

ip_pp_to_hs_and_or_values
  :: T.ImportPrefix -> ProgramPath -> P.IO (GTC.Haskell, PTC.MFOrValueIds)
ip_pp_to_hs_and_or_values = \ip pp ->
  read_prog_file pp >$> lc_to_hs_and_or_values ip

compile_file_to_svg_short :: ProgramPath -> P.IO ()
compile_file_to_svg_short = \pp ->
  compile_file_to_dot_short pp >> run_dot_and_remove_dot_file pp

compile_file_to_svg_full :: ProgramPath -> P.IO ()
compile_file_to_svg_full = \pp ->
  compile_file_to_dot_full pp >> run_dot_and_remove_dot_file pp

compile_file_to_prep_svg_full :: ProgramPath -> P.IO ()
compile_file_to_prep_svg_full = \pp ->
  compile_file_to_prep_dot_full pp >> run_dot_and_remove_dot_file pp

run_dot_and_remove_dot_file :: ProgramPath -> P.IO ()
run_dot_and_remove_dot_file pp =
  SP.callCommand
  ( "dot -T svg " ++ dot_file ++ " > " ++ H.make_extension "svg" pp ++
    " && rm " ++ dot_file
  )
  where
  dot_file :: ProgramPath
    = H.make_extension "dot" pp

compile_file_to_dot_short :: ProgramPath -> P.IO ()
compile_file_to_dot_short = \pp -> compile_file_to_dot compile_lc_to_dot_short pp

compile_file_to_dot_full :: ProgramPath -> P.IO ()
compile_file_to_dot_full = \pp -> compile_file_to_dot compile_lc_to_dot_full pp

compile_file_to_prep_dot_full :: ProgramPath -> P.IO ()
compile_file_to_prep_dot_full = \pp ->
  compile_file_to_dot compile_prep_to_dot_full pp

compile_file_to_dot :: (H.Lcases -> STC.Dot) -> ProgramPath -> P.IO ()
compile_file_to_dot = \cf pp ->
  compile_file_to_string cf pp >>= P.writeFile (H.make_extension "dot" pp)

compile_file_to_string :: CompileFunction -> ProgramPath -> P.IO P.String
compile_file_to_string = \cf pp -> read_prog_file pp >$> cf

read_prog_file :: ProgramPath -> P.IO H.Lcases
read_prog_file = H.add_dotlc_if_needed .> P.readFile

-- compiling to haskell

compile_lc_to_hs_with_ovis
  :: PTC.MFOrValueIds -> ThrowErrorOrDont -> H.Lcases -> GTC.Haskell
compile_lc_to_hs_with_ovis = \mf_ovis teod ->
  generate_to_compile (prog_to_hs_with_ovis mf_ovis, teod)

compile_lc_to_hs :: ThrowErrorOrDont -> H.Lcases -> GTC.Haskell
compile_lc_to_hs = \teod -> generate_to_compile (prog_to_hs, teod)

prog_to_hs_with_ovis :: PTC.MFOrValueIds -> T.Program -> GTC.Haskell
prog_to_hs_with_ovis = \mf_ovis prog ->
  PP.preprocess_prog_with_ovis mf_ovis prog &> GTC.to_haskell

prog_to_hs :: T.Program -> GTC.Haskell
prog_to_hs = PP.preprocess_prog .> GTC.to_haskell

-- MFOrValueIds related

empty_mf_ovis :: PTC.MFOrValueIds
empty_mf_ovis = (S.empty, M.empty)

lc_to_hs_and_or_values
  :: T.ImportPrefix -> H.Lcases -> (GTC.Haskell, PTC.MFOrValueIds)
lc_to_hs_and_or_values = \ip lc ->
  PH.parse lc &> \case
    P.Left err -> P.error $ error_to_str err
    P.Right prog -> (prog_to_hs prog, prog_to_mf_ovis ip prog)

prog_to_mf_ovis :: T.ImportPrefix -> T.Program -> PTC.MFOrValueIds
prog_to_mf_ovis = \ip -> PC.or_values .> ovis_mf_with_prefix ip

ovis_mf_with_prefix :: T.ImportPrefix -> PTC.OrValueIds -> PTC.MFOrValueIds
ovis_mf_with_prefix = \ip (eovi, fovim) ->
  (eorvi_mf_with_prefix ip eovi, forvim_mf_with_prefix ip fovim)

eorvi_mf_with_prefix
  :: T.ImportPrefix -> PTC.EmptyOrValueIds -> PTC.EmptyMFOrValueIds
eorvi_mf_with_prefix = \ip -> S.map (ovi_mf_with_prefix ip)

forvim_mf_with_prefix
  :: T.ImportPrefix -> PTC.FullOrValueIdsMap -> PTC.FullMFOrValueIdsMap
forvim_mf_with_prefix = \ip -> M.mapKeys (ovi_mf_with_prefix ip)

ovi_mf_with_prefix :: T.ImportPrefix -> PTC.OrValueId -> PTC.MFOrValueId
ovi_mf_with_prefix = \ip ovi -> T.MFSI (P.Just ip, ovi)

-- compiling to dot

compile_lc_to_dot_short :: H.Lcases -> STC.Dot
compile_lc_to_dot_short = generate_to_compile (SH.to_dot_final_short, Throw_err)

compile_lc_to_dot_full :: H.Lcases -> STC.Dot
compile_lc_to_dot_full = generate_to_compile (SH.to_dot_final_full, Throw_err)

compile_prep_to_dot_full :: H.Lcases -> STC.Dot
compile_prep_to_dot_full =
  generate_to_compile (prep_to_dot_final_full, Throw_err)

prep_to_dot_final_full :: T.Program -> STC.Dot
prep_to_dot_final_full = PP.preprocess_prog .> SH.to_dot_final_full

-- generate to compile

generate_to_compile :: GenerateTuple -> H.Lcases -> P.String
generate_to_compile = \(gen_f, teod) ->
  generate_to_compile_parse_err gen_f .> \case
    P.Left err -> throw_error_or_dont teod err
    P.Right a -> a

generate_to_compile_parse_err :: GenerateFunction -> H.Lcases -> ParseErrOrGenRes
generate_to_compile_parse_err = \gen -> PH.parse .> P.fmap gen

throw_error_or_dont :: ThrowErrorOrDont -> TP.ParseError -> P.String
throw_error_or_dont = \case
  Throw_err -> error_to_str .> P.error
  Dont_throw_err -> error_to_str

error_to_str :: TP.ParseError -> P.String
error_to_str = P.show .> ("Error :( ==> " ++)

-- collect import lines

source_to_import_lines :: H.Lcases -> [T.ImportLine]
source_to_import_lines =
  PH.parse .> \case
    P.Left err -> P.error $ error_to_str err
    P.Right p -> PC.collect_total_imls p

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
