{-
This file defines:
- types for parsing
- parsers that only affect the state for manipulating the indentation level and
  checking whether we are in the equal line of a value definition
- helper parsers for spaces, commas, equal symbols, arrows, parentheses etc
- parsers for char and string literals
-}

{-# language LambdaCase, FlexibleContexts #-}

module Parsing.TypesAndHelpers where

import Prelude ((<$>), (>), (&&), (/=), (++), ($), (<), (<*), (*>), (>>=), (+))
import Prelude ((>>))
import Prelude qualified as P
import Text.Parsec ((<?>), (<|>))
import Text.Parsec qualified as TP
import Text.Parsec.Token qualified as TPT

import Helpers ((>$>))
import Helpers qualified as H

-- types

type Parser = TP.Parsec P.String ParserState
type ParserState = (IndentationLevel, InEqualLine)
type IndentationLevel = P.Int
type InEqualLine = P.Bool

-- state parsers: indentation level

get_il :: Parser P.Int
get_il = P.fst <$> TP.getState

indent :: Parser ()
indent = get_il >$> H.ind_lvl_to_spaces >>= TP.string >> H.nothing

increase_il_by :: P.Int -> Parser ()
increase_il_by = \i -> TP.modifyState $ \(il, b) -> (il + i, b)

decrease_il_by :: P.Int -> Parser ()
decrease_il_by = \i -> increase_il_by (-i)

deeper_num :: P.Int -> Parser a -> Parser a
deeper_num = \i p -> increase_il_by i *> p <* decrease_il_by i

deeper :: Parser a -> Parser a
deeper = deeper_num 1

twice_deeper :: Parser a -> Parser a
twice_deeper = deeper_num 2

-- state parsers: equal line

are_we_in_equal_line :: Parser P.Bool
are_we_in_equal_line = P.snd <$> TP.getState

set_in_equal_line :: P.Bool -> Parser ()
set_in_equal_line = \b -> TP.modifyState (\(il, _) -> (il, b))

deeper_if_not_in_equal_line :: Parser a -> Parser a
deeper_if_not_in_equal_line = \parser ->
  are_we_in_equal_line >>= \case
    P.True -> parser
    P.False -> deeper parser

-- helper parsers

[nl, nl_indent, space_or_nl, opt_space, comma, equals]
  = [ TP.many (TP.char ' ' <|> TP.char '\t') *> TP.char '\n' *> H.nothing
    , nl *> indent
    , (TP.try (nl *> TP.string "  ") <|> TP.string " ") *> H.nothing
    , TP.optional (TP.char ' ')
    , TP.char ',' *> opt_space
    , opt_space_around (TP.string "=") *> H.nothing
    ]
  :: [Parser ()]

[underscore, lower_under]
  = [TP.char '_', TP.lower <|> underscore]
  :: [Parser P.Char]

[digits, func_arr]
  = [TP.many1 TP.digit, opt_space *> TP.string "=>"]
  :: [Parser P.String]

has_type_symbol :: Parser ()
has_type_symbol =
  (TP.try nl_indent *> TP.string ": " <|> opt_space_around (TP.string ":")) *>
  H.nothing

opt_space_around :: Parser a -> Parser a
opt_space_around = \a -> opt_space *> a <* opt_space

in_paren :: Parser a -> Parser a
in_paren = \a -> TP.char '(' *> opt_space_around a <* TP.char ')'

err_if_less_than_2 :: P.Integer -> Parser P.Integer
err_if_less_than_2 =  \i -> case (i < 2) of
  P.True -> TP.unexpected "integer in power type must be greater than 1"
  P.False -> P.return i

-- reserved words

reserved_words :: [P.String]
reserved_words = ["cases", "where"]

err_if_reserved :: P.String -> Parser ()
err_if_reserved = \id_str -> case P.elem id_str reserved_words of
  P.True -> TP.unexpected $ "cannot use " ++ id_str ++ " as an identifier"
  P.False -> H.nothing

-- for literals. Had to copy from Text.Parsec.Token source code
-- because I didn't want spaces after the char and string literals ...
-- if anyone knows how to import hidden functions from a module plz let me know
-- (this could have been way faster)

-- char literal

charLiteral :: Parser P.Char
charLiteral =
  (TP.between (TP.char '\'') (TP.char '\'' <?> "end of character") characterChar)
  <?> "character"

characterChar :: Parser P.Char
characterChar = charLetter <|> charEscape <?> "literal character"

charLetter :: Parser P.Char
charLetter = TP.satisfy (\c -> (c /= '\'') && (c /= '\\') && (c > '\026'))

charEscape :: Parser P.Char
charEscape = do{ _ <- TP.char '\\'; charEsc <?> "escape code" }

charEsc :: Parser P.Char
charEsc =
  TP.choice (P.map parseEsc escMap)
  where
  parseEsc (c, code) = do{ _ <- TP.char c; P.return code }
  escMap = P.zip ("ntr0\\\"\'") ("\n\t\r\0\\\"\'")

-- string literal

stringLiteral :: Parser P.String
stringLiteral =
  TP.between (TP.char '"') (TP.char '"' <?> "end of string") (TP.many stringChar)
  <?> "string literal"

stringChar :: Parser P.Char
stringChar = stringLetter <|> charEscape <?> "string character"

stringLetter :: Parser P.Char
stringLetter = TP.satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

{-
For fast vim file navigation:
AST.hs
-}
