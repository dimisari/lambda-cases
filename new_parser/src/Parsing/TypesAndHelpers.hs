{-# LANGUAGE FlexibleContexts #-}

module Parsing.TypesAndHelpers where

import Text.Parsec
import Text.Parsec.Token hiding (comma, charLiteral, stringLiteral)

import Helpers

type Parser = Parsec String ParserState
type ParserState = (IndentationLevel, InEqualLine)
type IndentationLevel = Int
type InEqualLine = Bool

-- state parsers: indentation level
get_il :: Parser Int
get_il = fst <$> getState

indent :: Parser ()
indent = get_il >$> ind_lvl_to_spaces >>= string >> nothing

modify_il :: (Int -> Int) -> Parser ()
modify_il = \f -> modifyState $ \(il, b) -> (f il, b)

increase_il_by :: Int -> Parser ()
increase_il_by = \i -> modify_il (+i)

decrease_il_by :: Int -> Parser ()
decrease_il_by = \i -> modify_il $ \il -> il - i

deeper_num :: Int -> Parser a -> Parser a
deeper_num = \i p -> increase_il_by i *> p <* decrease_il_by i

deeper :: Parser a -> Parser a
deeper = deeper_num 1

twice_deeper :: Parser a -> Parser a
twice_deeper = deeper_num 2

-- state parsers: equal line
are_we_in_equal_line :: Parser Bool
are_we_in_equal_line = snd <$> getState

set_in_equal_line :: Bool -> Parser ()
set_in_equal_line = \b -> modifyState (\(il, _) -> (il, b))

deeper_if_not_in_equal_line :: Parser a -> Parser a
deeper_if_not_in_equal_line = \parser ->
  are_we_in_equal_line >>= \case
    True -> parser
    False -> deeper parser

-- helper parsers
[nl, nl_indent, space_or_nl, opt_space, comma, equals]
  = [ many (char ' ' <|> char '\t') *> char '\n' *> nothing
    , nl *> indent
    , (try (nl *> string "  ") <|> string " ") *> nothing
    , optional (char ' ')
    , char ',' *> opt_space
    , opt_space_around (string "=") *> nothing
    ]
  :: [Parser ()]

[underscore, lower_under]
  = [char '_', lower <|> underscore]
  :: [Parser Char]

[digits, func_arr]
  = [many1 digit, opt_space *> string "=>"]
  :: [Parser String]

has_type_symbol :: Parser ()
has_type_symbol =
  (try nl_indent *> string ": " <|> opt_space_around (string ":")) *> nothing

opt_space_around :: Parser a -> Parser a
opt_space_around = \a -> opt_space *> a <* opt_space

in_paren :: Parser a -> Parser a
in_paren = \a -> char '(' *> opt_space_around a <* char ')'

err_if_less_than_2 :: Int -> Parser Int
err_if_less_than_2 =  \i -> case (i < 2) of
  True -> unexpected "integer in power type must be greater than 1"
  False -> return i

-- reserved words
reserved_words :: [String]
reserved_words = ["cases", "where"]

err_if_reserved :: String -> Parser ()
err_if_reserved = \id_str -> case elem id_str reserved_words of
  True -> unexpected $ "cannot use " ++ id_str ++ " as an identifier"
  False -> nothing

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

-- ASTTypes.hs
-- AST.hs
-- Test.hs
