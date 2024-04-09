{-# LANGUAGE FlexibleContexts #-}

module Parsing.TypesAndHelpers where

import Text.Parsec
import Text.Parsec.Token hiding (comma, charLiteral, stringLiteral)

import Helpers

type Parser = Parsec String ParserState
type ParserState = (IndentationLevel, InEqualLine)
type IndentationLevel = Int
type InEqualLine = Bool

-- helpers for state
increase_il_by :: Int -> Parser ()
increase_il_by = \i -> modifyState (\(il, b) -> (il + i, b))

decrease_il_by :: Int -> Parser ()
decrease_il_by = \i -> modifyState (\(il, b) -> (il - i, b))

deeper :: Parser a -> Parser a
deeper p = increase_il_by 1 *> p <* decrease_il_by 1

deeper2 :: Parser a -> Parser a
deeper2 p = increase_il_by 2 *> p <* decrease_il_by 2

we_are_in_equal_line :: Parser ()
we_are_in_equal_line = modifyState (\(il, _) -> (il, True))

we_are_not_in_equal_line :: Parser ()
we_are_not_in_equal_line = modifyState (\(il, _) -> (il, False))

are_we_in_equal_line :: Parser Bool
are_we_in_equal_line = snd <$> getState

inc_il_if_false :: Bool -> Parser ()
inc_il_if_false = \case
  True -> return ()
  False -> increase_il_by 1

dec_il_if_false :: Bool -> Parser ()
dec_il_if_false = \case
  True -> return ()
  False -> decrease_il_by 1

-- helper parsers
[nl, nl_indent, space_or_nl, opt_space, comma]
  = [ many (char ' ' <|> char '\t') *> char '\n' *> return ()
    , nl *> indent
    , (try (nl *> string "  ") <|> string " ") *> return ()
    , optional (char ' ')
    , char ',' *> opt_space
    ]
  :: [Parser ()]

[underscore, lower_under]
  = [char '_', lower <|> underscore]
  :: [Parser Char]

[digits, func_arr]
  = [many1 digit, opt_space *> string "=>"]
  :: [Parser String]

indent :: Parser ()
indent =
  getState >>= \(il, _) -> string (concat $ replicate il "  ") >> return ()

has_type_symbol :: Parser ()
has_type_symbol =
  (try nl_indent *> string ": " <|> opt_space_around (string ":")) *> return ()

opt_space_around :: Parser a -> Parser a
opt_space_around = \a -> opt_space *> a <* opt_space

in_paren :: Parser a -> Parser a
in_paren = \a -> char '(' *> opt_space_around a <* char ')'

-- for literals. Had to copy from Text.Parsec.Token source code
-- because I didn't want spaces after the char and string literals ...
-- if anyone knows how to import hidden functions from a module plz let me know
-- (this could have been way faster)

-- char literal
charLiteral :: Parser Char
charLiteral =
  (between (char '\'') (char '\'' <?> "end of character") characterChar)
  <?> "character"

characterChar :: Parser Char
characterChar = charLetter <|> charEscape <?> "literal character"

charLetter :: Parser Char
charLetter = satisfy (\c -> (c /= '\'') && (c /= '\\') && (c > '\026'))

charEscape :: Parser Char
charEscape = do{ _ <- char '\\'; charEsc <?> "escape code" }

charEsc :: Parser Char
charEsc =
  choice (map parseEsc escMap)
  where
  parseEsc (c, code) = do{ _ <- char c; return code }
  escMap = zip ("ntr0\\\"\'") ("\n\t\r\0\\\"\'")

-- string literal
stringLiteral :: Parser String
stringLiteral =
  between (char '"') (char '"' <?> "end of string") (many stringChar)
  <?> "string literal"

stringChar :: Parser Char
stringChar = stringLetter <|> charEscape <?> "string character"

stringLetter :: Parser Char
stringLetter = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

-- AST.hs
-- Test.hs
