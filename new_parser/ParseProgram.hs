{-# LANGUAGE LambdaCase,
TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}

module ParseProgram where

import Text.Parsec (runParser, eof, ParseError)

import System.Directory

import ASTTypes
import Parsers
import ShowInstances

-- helpers

(.>) = flip (.)

type FileName = String
type ProgramStr = String
type ParseResult = String

(programs_dir, parse_res_dir) =
  ("programs/", "programs_parse_res/")
  :: (FilePath, FilePath)

-- main

main :: IO ()
main = listDirectory programs_dir >>= mapM_ read_parse_write

read_parse_write :: FileName -> IO ()
read_parse_write file_name =
  readFile input_path >>= parse_program .> writeFile output_path
  where
  input_path :: FilePath
  input_path = programs_dir ++ file_name

  output_path :: FilePath
  output_path = parse_res_dir ++ file_name

parse_program :: ProgramStr -> ParseResult
parse_program =
  runParser (parser :: Parser Program) (0, False) "" .> \case
    Left err -> "Error :( ==>\n" ++ show err ++ "\n\n"
    Right res -> "Parsed :) ==>\n" ++ show res ++ "\n\n"
