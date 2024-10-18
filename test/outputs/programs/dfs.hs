{-# language FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, UndecidableInstances, FlexibleContexts #-}
import qualified Prelude as P
import PredefImports.Predefined
import PredefImports.OpsInHaskell

data A'Tree a1 =
  A'Tree' { root :: a1, subtrees :: A'Trees a1 }

instance FromTuple2 a1 (A'Trees a1) (A'Tree a1) where
  ft2 = \(x1, x2) -> A'Tree' x1 x2

c0root :: a1 -> A'Tree a1 -> A'Tree a1
c0subtrees :: A'Trees a1 -> A'Tree a1 -> A'Tree a1
c0root = \new x -> x { root = new }
c0subtrees = \new x -> x { subtrees = new }

type A'Trees a1 = ListOf's (A'Tree a1)

data A'ResultTreeAndNum a1 =
  A'ResultTreeAndNum' { tree :: A'Tree (P.Integer, a1), t_num :: P.Integer }

instance FromTuple2 (A'Tree (P.Integer, a1)) P.Integer (A'ResultTreeAndNum a1) where
  ft2 = \(x1, x2) -> A'ResultTreeAndNum' x1 x2

c0tree :: A'Tree (P.Integer, a1) -> A'ResultTreeAndNum a1 -> A'ResultTreeAndNum a1
c0t_num :: P.Integer -> A'ResultTreeAndNum a1 -> A'ResultTreeAndNum a1
c0tree = \new x -> x { tree = new }
c0t_num = \new x -> x { t_num = new }

data A'ResultTreesAndNum a1 =
  A'ResultTreesAndNum' { trees :: A'Trees (P.Integer, a1), ts_num :: P.Integer }

instance FromTuple2 (A'Trees (P.Integer, a1)) P.Integer (A'ResultTreesAndNum a1) where
  ft2 = \(x1, x2) -> A'ResultTreesAndNum' x1 x2

c0trees :: A'Trees (P.Integer, a1) -> A'ResultTreesAndNum a1 -> A'ResultTreesAndNum a1
c0ts_num :: P.Integer -> A'ResultTreesAndNum a1 -> A'ResultTreesAndNum a1
c0trees = \new x -> x { trees = new }
c0ts_num = \new x -> x { ts_num = new }

dfs_on_tree' :: forall a1. A'Tree a1 -> A'Tree (P.Integer, a1)
dfs_on_tree' =
  let
  dfs_on_tree'with_num' :: (A'Tree a1, P.Integer) -> A'ResultTreeAndNum a1
  dfs_on_tree'with_num' =
    \(tree, num) ->
    let
    new_tree :: A'Tree (P.Integer, a1)
    new_tree =
      ft2(ft2(num, root(tree)), trees(subtrees_res))

    subtrees_res :: A'ResultTreesAndNum a1
    subtrees_res =
      dfs_on_trees'with_num'(subtrees(tree), num !+ (1 :: P.Integer))
    in
    ft2(new_tree, ts_num(subtrees_res))

  dfs_on_trees'with_num' :: (A'Trees a1, P.Integer) -> A'ResultTreesAndNum a1
  dfs_on_trees'with_num' =
    \(pA0, num) ->
    case pA0 of
      [] -> ft2([], num)
      tree1 : other_trees ->
        let
        tree_res :: A'ResultTreeAndNum a1
        tree_res =
          dfs_on_tree'with_num'(tree1, num)

        trees_res :: A'ResultTreesAndNum a1
        trees_res =
          dfs_on_trees'with_num'(other_trees, t_num(tree_res))
        in
        ft2(tree(tree_res) !+ trees(trees_res), ts_num(trees_res))
  in
  (\pA0 -> dfs_on_tree'with_num'(pA0, (1 :: P.Integer))) .> (\x' -> tree(x'))

test_tree :: A'Tree P.Char
test_tree =
  ft2
  ( 'a'
  , [ft2('b', [ft2('e', []), ft2('f', [])]), ft2('c', []), ft2('d', [ft2('g', []), ft2('h', [])])]
  )

instance P.Show b0 => P.Show (A'Tree b0) where
  show =
    \tree ->
    "\nroot: " !+ P.show(root(tree)) !+ 
    "\nsubtrees:\n" !+ P.show(subtrees(tree)) !+ "\n"

main :: IO
main =
  dfs_on_tree'(test_tree) &> (\pA0 -> P.show(pA0)) &> (\pA0 -> print'(pA0))