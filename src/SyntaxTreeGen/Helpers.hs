{-# language LambdaCase, UndecidableInstances, IncoherentInstances #-}

module SyntaxTreeGen.Helpers where

import Prelude ((++), (.), ($))
import Prelude qualified as P
import Helpers ((.>))
import SyntaxTreeGen.TypesAndClasses qualified as STC

add_new_root :: STC.AddNewRootCode a => P.String -> a -> (STC.Root, STC.Dot)
add_new_root = \s a -> (s, STC.add_new_root_code s a)

instance STC.AddNewRootCode a => STC.AddNewRootCode [a] where
  add_new_root_code = \s as -> P.concatMap (STC.add_new_root_code s) as

instance
  (STC.AddNewRootCode a, STC.AddNewRootCode b) => STC.AddNewRootCode (a, b)
  where
  add_new_root_code = \s (a, b) ->
    STC.add_new_root_code s a ++ STC.add_new_root_code s b

instance
  (STC.AddNewRootCode a, STC.AddNewRootCode b, STC.AddNewRootCode c) =>
  STC.AddNewRootCode (a, b, c)
  where
  add_new_root_code = \s (a, b, c) ->
    STC.add_new_root_code s a ++ STC.add_new_root_code s b ++
    STC.add_new_root_code s c

instance
  ( STC.AddNewRootCode a, STC.AddNewRootCode b, STC.AddNewRootCode c
  , STC.AddNewRootCode d
  ) =>
  STC.AddNewRootCode (a, b, c, d)
  where
  add_new_root_code = \s (a, b, c, d) ->
    STC.add_new_root_code s a ++ STC.add_new_root_code s b ++
    STC.add_new_root_code s c ++ STC.add_new_root_code s d

instance STC.AddNewRootCode a => STC.AddNewRootCode (P.Maybe a) where
  add_new_root_code = \s -> \case
    P.Just a -> STC.add_new_root_code s a
    P.Nothing -> ""

instance STC.ToDot a => STC.AddNewRootCode a where
  add_new_root_code = \s a -> new_and_old_root_edge s a ++ get_dot_code a

new_and_old_root_edge :: STC.ToDot a => P.String -> a -> P.String
new_and_old_root_edge = \s a -> s ++ " -- " ++ get_root a ++ "\n"

get_root :: STC.ToDot a => a -> P.String
get_root = P.fst . STC.to_dot

get_dot_code :: STC.ToDot a => a -> P.String
get_dot_code = P.snd . STC.to_dot

connect_node_with_new_root :: P.String -> P.String -> (P.String, P.String)
connect_node_with_new_root = \r s -> (r, r ++ " -- " ++ s)

string_to_node_string :: P.String -> P.String
string_to_node_string = escape_spaces .> add_quotes

escape_spaces :: P.String -> P.String
escape_spaces = \case
  [] -> []
  ' ' : s -> "\\ " ++ escape_spaces s
  c : s -> [c] ++ escape_spaces s

add_quotes :: P.String -> P.String
add_quotes = \s -> "\"" ++ s ++ "\""
