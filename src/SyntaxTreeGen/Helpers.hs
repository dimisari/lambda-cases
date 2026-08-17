{-# language LambdaCase #-}

module SyntaxTreeGen.Helpers where

import Prelude ((++), (.))
import Prelude qualified as P
import Helpers ((.>))
import SyntaxTreeGen.TypesAndClasses qualified as STC

collect_and_connect_with_roots
  :: STC.ToDot a => P.String -> [a] -> (STC.Root, STC.Dot)
collect_and_connect_with_roots = \s as ->
  (s, connect_with_roots s as ++ collect_dot_code as)

add_connect_with_root :: STC.ToDot a => P.String -> a -> (STC.Root, STC.Dot)
add_connect_with_root = \s a -> (s, connect_with_root s a ++ get_dot_code a)

connect_with_roots :: STC.ToDot a => P.String -> [a] -> P.String
connect_with_roots = \s -> P.concatMap (connect_with_root s)

connect_with_root :: STC.ToDot a => P.String -> a -> P.String
connect_with_root = \s a -> s ++ " -- " ++ get_root a ++ "\n"

collect_dot_code :: STC.ToDot a => [a] -> P.String
collect_dot_code = P.concatMap get_dot_code

get_root :: STC.ToDot a => a -> P.String
get_root = P.fst . STC.to_dot

get_dot_code :: STC.ToDot a => a -> P.String
get_dot_code = P.snd . STC.to_dot

make_root_and_connect :: P.String -> P.String -> (P.String, P.String)
make_root_and_connect = \r s -> (r, r ++ " -- " ++ s)

make_node_string :: P.String -> P.String
make_node_string = escape_spaces .> add_quotes

escape_spaces :: P.String -> P.String
escape_spaces = \case
  [] -> []
  ' ' : s -> "\\ " ++ escape_spaces s
  c : s -> [c] ++ escape_spaces s

add_quotes :: P.String -> P.String
add_quotes = \s -> "\"" ++ s ++ "\""
