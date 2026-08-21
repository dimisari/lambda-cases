{-# language LambdaCase, UndecidableInstances, IncoherentInstances #-}

module SyntaxTreeGen.Helpers where

import Prelude ((+), (++), (.), ($), (>>=))
import Prelude qualified as P
import Control.Monad qualified as CMSL
import Control.Monad.State.Lazy qualified as CMSL

import Helpers ((.>), (>$>), (&>))
import SyntaxTreeGen.TypesAndClasses qualified as STC

to_dot_final :: STC.ToStringTree a => a -> STC.Dot
to_dot_final =
  STC.to_string_tree .> remove_single_child_parents .> string_tree_to_dot .>
  \(root, dot) -> "graph " ++ root ++ "\n{\n" ++ dot ++ "}"

-- string tree to dot

string_tree_to_dot :: STC.StringTree -> STC.DotTuple
string_tree_to_dot = \st -> CMSL.evalState (state_string_tree_to_dot st) 0

state_string_tree_to_dot :: STC.StringTree -> STC.NumState STC.DotTuple
state_string_tree_to_dot = \(STC.ST (r, sts)) ->
  get_new_node_with_label r >>= \(nr, r_code) ->
  state_string_trees_to_dot sts >>= \(subtrees_roots, subtrees_codes) ->
  P.pure
  (nr, r_code ++ connect_with_all nr subtrees_roots ++ P.concat subtrees_codes)

state_string_trees_to_dot
  :: [STC.StringTree] -> STC.NumState ([STC.Root], [STC.Dot])
state_string_trees_to_dot = P.mapM state_string_tree_to_dot .> P.fmap P.unzip

-- get node with label

get_new_num :: STC.NumState P.Int
get_new_num = CMSL.withState (+ 1) CMSL.get

get_new_node :: STC.NumState P.String
get_new_node = P.fmap (P.show .> ("n" ++)) get_new_num

get_new_node_with_label :: P.String -> STC.NumState (STC.NodeName, STC.LabelCode)
get_new_node_with_label = \l ->
  P.fmap (\n -> (n, node_and_label_to_label_code n l)) get_new_node

node_and_label_to_label_code :: P.String -> P.String -> P.String
node_and_label_to_label_code = \n l ->
  n ++ " [label=\"" ++ escape_string_quotes l ++ "\"]\n"

escape_string_quotes :: P.String -> P.String
escape_string_quotes = P.concatMap escape_string_quote

escape_string_quote :: P.Char -> P.String
escape_string_quote = \case
  '\"' -> "\\\""
  c -> [c]

-- connect

connect :: P.String -> P.String -> P.String
connect = \r s -> r ++ " -- " ++ s ++ "\n"

connect_with_all :: P.String -> [P.String] -> P.String
connect_with_all = \r l -> P.concatMap (connect r) l

-- to string tree

add_new_root :: STC.ToStringTrees a => P.String -> a -> STC.StringTree
add_new_root = \s a -> STC.ST (s, STC.to_string_trees a)

root_and_node_to_tree :: P.String -> P.String -> STC.StringTree
root_and_node_to_tree = \r n -> STC.ST (r, [STC.ST (n, [])])

instance STC.ToStringTree P.Int where
  to_string_tree = \i -> STC.to_string_tree $ P.show i

instance STC.ToStringTree P.Integer where
  to_string_tree = \i -> STC.to_string_tree $ P.show i

instance STC.ToStringTree P.Double where
  to_string_tree = \d -> STC.to_string_tree $ P.show d

instance STC.ToStringTree P.Char where
  to_string_tree = \c -> STC.to_string_tree [c]

instance STC.ToStringTree P.String where
  to_string_tree = \s -> STC.ST (s, [])

instance STC.ToStringTrees P.String where
  to_string_trees = STC.to_string_tree .> \x -> [x]

instance STC.ToStringTrees a => STC.ToStringTrees [a] where
  to_string_trees = P.concatMap STC.to_string_trees

instance
  (STC.ToStringTrees a, STC.ToStringTrees b) => STC.ToStringTrees (a, b)
  where
  to_string_trees = \(a, b) -> STC.to_string_trees a ++ STC.to_string_trees b

instance
  (STC.ToStringTrees a, STC.ToStringTrees b, STC.ToStringTrees c) =>
  STC.ToStringTrees (a, b, c)
  where
  to_string_trees = \(a, b, c) ->
    STC.to_string_trees a ++ STC.to_string_trees b ++ STC.to_string_trees c

instance
  ( STC.ToStringTrees a, STC.ToStringTrees b, STC.ToStringTrees c
  , STC.ToStringTrees d
  ) =>
  STC.ToStringTrees (a, b, c, d)
  where
  to_string_trees = \(a, b, c, d) ->
    STC.to_string_trees a ++ STC.to_string_trees b ++ STC.to_string_trees c ++
    STC.to_string_trees d

instance
  ( STC.ToStringTrees a, STC.ToStringTrees b, STC.ToStringTrees c
  , STC.ToStringTrees d, STC.ToStringTrees e
  ) =>
  STC.ToStringTrees (a, b, c, d, e)
  where
  to_string_trees = \(a, b, c, d, e) ->
    STC.to_string_trees a ++ STC.to_string_trees b ++ STC.to_string_trees c ++
    STC.to_string_trees d ++ STC.to_string_trees e

instance STC.ToStringTrees a => STC.ToStringTrees (P.Maybe a) where
  to_string_trees = \case
    P.Just a -> STC.to_string_trees a
    P.Nothing -> []

instance STC.ToStringTree a => STC.ToStringTrees a where
  to_string_trees = STC.to_string_tree .> \x -> [x]

-- remove single child parents

remove_single_child_parents :: STC.StringTree -> STC.StringTree
remove_single_child_parents (STC.ST (root, sts)) =
  STC.ST (root, new_sts)
  where
  new_sts :: [STC.StringTree]
  new_sts =
    P.map (remove_single_child_parents .> replace_with_child_if_only_one) sts



replace_with_child_if_only_one :: STC.StringTree -> STC.StringTree
replace_with_child_if_only_one = \t@(STC.ST (root, children)) ->
  case children of
    [child] -> child
    _ -> t
