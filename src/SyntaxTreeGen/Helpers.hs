{-# language LambdaCase, UndecidableInstances, IncoherentInstances #-}

module SyntaxTreeGen.Helpers where

import Prelude ((+), (++), (.), ($), (>>=))
import Prelude qualified as P
import Control.Monad.State.Lazy qualified as CMSL

import Helpers ((.>), (>$>), (&>))
import SyntaxTreeGen.TypesAndClasses qualified as STC

to_dot_final :: STC.ToDot a => a -> STC.Dot
to_dot_final =
  STC.to_dot .> \a_to_dot ->
  CMSL.evalState a_to_dot 0 &> \(root, dot) ->
  "graph " ++ root ++ "\n{\n" ++ dot ++ "}"

add_new_root :: STC.AddNewRootCode a => P.String -> a -> STC.NumStateDotTuple
add_new_root = \s a ->
  get_new_node_with_label s >>= \(n, label_code) ->
  STC.add_new_root_code n a >>= \code ->
  P.pure (n, label_code ++ code)

instance STC.AddNewRootCode P.Char where
  add_new_root_code = \s c -> connect_new_node_with_root s [c]

instance STC.AddNewRootCode P.Integer where
  add_new_root_code = \s i -> connect_new_node_with_root s $ P.show i

instance STC.AddNewRootCode P.String where
  add_new_root_code = \s1 s2 -> connect_new_node_with_root s1 s2

instance STC.AddNewRootCode a => STC.AddNewRootCode [a] where
  add_new_root_code = \s as ->
    concat_dot_code $ P.map (STC.add_new_root_code s) as

instance
  (STC.AddNewRootCode a, STC.AddNewRootCode b) => STC.AddNewRootCode (a, b)
  where
  add_new_root_code = \s (a, b) ->
    concat_dot_code [STC.add_new_root_code s a, STC.add_new_root_code s b]

instance
  (STC.AddNewRootCode a, STC.AddNewRootCode b, STC.AddNewRootCode c) =>
  STC.AddNewRootCode (a, b, c)
  where
  add_new_root_code = \s (a, b, c) ->
    concat_dot_code
      [ STC.add_new_root_code s a, STC.add_new_root_code s b
      , STC.add_new_root_code s c
      ]

instance
  ( STC.AddNewRootCode a, STC.AddNewRootCode b, STC.AddNewRootCode c
  , STC.AddNewRootCode d
  ) =>
  STC.AddNewRootCode (a, b, c, d)
  where
  add_new_root_code = \s (a, b, c, d) ->
    concat_dot_code
      [ STC.add_new_root_code s a, STC.add_new_root_code s b
      , STC.add_new_root_code s c, STC.add_new_root_code s d
      ]

instance
  ( STC.AddNewRootCode a, STC.AddNewRootCode b, STC.AddNewRootCode c
  , STC.AddNewRootCode d, STC.AddNewRootCode e
  ) =>
  STC.AddNewRootCode (a, b, c, d, e)
  where
  add_new_root_code = \s (a, b, c, d, e) ->
    concat_dot_code
      [ STC.add_new_root_code s a, STC.add_new_root_code s b
      , STC.add_new_root_code s c, STC.add_new_root_code s d
      , STC.add_new_root_code s e
      ]

instance STC.AddNewRootCode a => STC.AddNewRootCode (P.Maybe a) where
  add_new_root_code = \s -> \case
    P.Just a -> STC.add_new_root_code s a
    P.Nothing -> P.pure ""

instance STC.ToDot a => STC.AddNewRootCode a where
  add_new_root_code = \s a ->
    STC.to_dot a >>= \(root, dot) -> P.pure $ connect s root ++ dot

concat_dot_code :: [STC.NumState STC.Dot] -> STC.NumState STC.Dot
concat_dot_code = P.fmap P.concat . P.sequence

-- get node with label

get_new_num :: STC.NumState P.Int
get_new_num = CMSL.withState (+ 1) CMSL.get

get_new_node :: STC.NumState P.String
get_new_node = P.fmap (P.show .> ("n" ++)) get_new_num

get_new_node_with_label :: P.String -> STC.NumState (STC.NodeName, STC.LabelCode)
get_new_node_with_label = \l ->
  P.fmap (\n -> (n, node_and_label_to_label_code n l)) get_new_node

node_and_label_to_label_code :: P.String -> P.String -> P.String
node_and_label_to_label_code = \n l -> n ++ " [label=\"" ++ l ++ "\"]\n"

-- connect new root and new node

connect_node_with_new_root :: P.String -> P.String -> STC.NumStateDotTuple
connect_node_with_new_root = \r s ->
  get_new_node_with_label r >>= \(nr, r_code) ->
  get_new_node_with_label s >>= \(ns, s_code) ->
  P.pure (nr, r_code ++ s_code ++ connect nr ns)

connect_new_node_with_root :: P.String -> P.String -> STC.NumState P.String
connect_new_node_with_root = \r n ->
  P.fmap (\(nn, n_code) -> n_code ++ connect r nn) $ get_new_node_with_label n

connect :: P.String -> P.String -> P.String
connect = \r s -> r ++ " -- " ++ s ++ "\n"

escape_spaces :: P.String -> P.String
escape_spaces = \case
  [] -> []
  ' ' : s -> "\\ " ++ escape_spaces s
  c : s -> [c] ++ escape_spaces s

add_quotes :: P.String -> P.String
add_quotes = \s -> "\"" ++ s ++ "\""
