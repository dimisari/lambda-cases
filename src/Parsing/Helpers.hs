{-
This file defines:
- types for parsing
- parsers that only affect the state for manipulating the indentation level and
  checking whether we are in the equal line of a value definition
- helper parsers for spaces, commas, equal symbols, arrows, parentheses etc
- parsers for char and string literals
-}

{-# language LambdaCase, FlexibleContexts #-}

module Parsing.Helpers where

import Prelude ((<$>), (>), (&&), (/=), (++), ($), (<), (<*), (*>), (>>=), (+))
import Prelude ((>>))
import Prelude qualified as P
import Text.Parsec ((<?>), (<|>))
import Text.Parsec qualified as TP
import Text.Parsec.Token qualified as TPT

import Helpers ((>$>), (>++<))
import Helpers qualified as H
import Parsing.TypesAndClasses qualified as PTC

-- state parsers: indentation level

get_il :: PTC.Parser P.Int
get_il = P.fst <$> TP.getState

indent :: PTC.Parser ()
indent = get_il >$> H.ind_lvl_to_spaces >>= TP.string >> H.nothing

increase_il_by :: P.Int -> PTC.Parser ()
increase_il_by = \i -> TP.modifyState $ \(il, b) -> (il + i, b)

decrease_il_by :: P.Int -> PTC.Parser ()
decrease_il_by = \i -> increase_il_by (-i)

deeper_num :: P.Int -> PTC.Parser a -> PTC.Parser a
deeper_num = \i p -> increase_il_by i *> p <* decrease_il_by i

deeper :: PTC.Parser a -> PTC.Parser a
deeper = deeper_num 1

twice_deeper :: PTC.Parser a -> PTC.Parser a
twice_deeper = deeper_num 2

-- state parsers: equal line

are_we_in_equal_line :: PTC.Parser P.Bool
are_we_in_equal_line = P.snd <$> TP.getState

set_in_equal_line :: P.Bool -> PTC.Parser ()
set_in_equal_line = \b -> TP.modifyState (\(il, _) -> (il, b))

deeper_if_not_in_equal_line :: PTC.Parser a -> PTC.Parser a
deeper_if_not_in_equal_line = \parser ->
  are_we_in_equal_line >>= \case
    P.True -> parser
    P.False -> deeper parser

-- helper parsers

[nl, nl_nl, nl_indent, nl_d_sp, space_or_nl, opt_space, comma, equals]
  = [ TP.many (TP.char ' ' <|> TP.char '\t') *> TP.char '\n' *> H.nothing
    , nl *> nl
    , nl *> indent
    , nl <* TP.string "  "
    , (TP.try nl_d_sp <|> (TP.string " " *> H.nothing))
    , TP.optional (TP.char ' ')
    , TP.char ',' *> opt_space
    , opt_space_around (TP.string "=") *> H.nothing
    ]
  :: [PTC.Parser ()]

[underscore, lower_under]
  = [TP.char '_', TP.lower <|> underscore]
  :: [PTC.Parser P.Char]

[digits, func_arr]
  = [TP.many1 TP.digit, opt_space_around (TP.string "=>")]
  :: [PTC.Parser P.String]

non_zero_digit :: PTC.Parser P.String
non_zero_digit =
  TP.digit >>= \case
    '0' -> TP.unexpected "zero digit"
    c -> P.pure [c]

int_str_p :: PTC.Parser P.String
int_str_p =
  TP.string "0" <|>
  TP.option "" (TP.string "-") >++< non_zero_digit >++< TP.many TP.digit

has_type :: PTC.Parser ()
has_type = (TP.try nl_indent <|> opt_space ) *> TP.string ":" *> opt_space

opt_space_around :: PTC.Parser a -> PTC.Parser a
opt_space_around = \a -> opt_space *> a <* opt_space

in_paren :: PTC.Parser a -> PTC.Parser a
in_paren = \a -> TP.char '(' *> opt_space_around a <* TP.char ')'

err_if_less_than_2 :: P.Integer -> PTC.Parser P.Integer
err_if_less_than_2 =  \i -> case (i < 2) of
  P.True -> TP.unexpected "integer in power type must be greater than 1"
  P.False -> P.return i

-- reserved words

reserved_words :: [P.String]
reserved_words = ["cases", "where"]

err_if_reserved :: P.String -> PTC.Parser ()
err_if_reserved = \id_str -> case P.elem id_str reserved_words of
  P.True -> TP.unexpected $ "cannot use " ++ id_str ++ " as an identifier"
  P.False -> H.nothing

-- for literals. Had to copy from Text.Parsec.Token source code
-- because I didn't want spaces after the char and string literals ...
-- if anyone knows how to import hidden functions from a module plz let me know
-- (this could have been way faster)

-- char literal

charLiteral :: PTC.Parser P.Char
charLiteral =
  (TP.between (TP.char '\'') (TP.char '\'' <?> "end of character") characterChar)
  <?> "character"

characterChar :: PTC.Parser P.Char
characterChar = charLetter <|> charEscape <?> "literal character"

charLetter :: PTC.Parser P.Char
charLetter = TP.satisfy (\c -> (c /= '\'') && (c /= '\\') && (c > '\026'))

charEscape :: PTC.Parser P.Char
charEscape = do{ _ <- TP.char '\\'; charEsc <?> "escape code" }

charEsc :: PTC.Parser P.Char
charEsc =
  TP.choice (P.map parseEsc escMap)
  where
  parseEsc (c, code) = do{ _ <- TP.char c; P.return code }
  escMap = P.zip ("ntr0\\\"\'") ("\n\t\r\0\\\"\'")

-- string literal

stringLiteral :: PTC.Parser P.String
stringLiteral =
  TP.between (TP.char '"') (TP.char '"' <?> "end of string") (TP.many stringChar)
  <?> "string literal"

stringChar :: PTC.Parser P.Char
stringChar = stringLetter <|> charEscape <?> "string character"

stringLetter :: PTC.Parser P.Char
stringLetter = TP.satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

{-
For fast vim file navigation:
AST.hs
-}
