{-# language FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, UndecidableInstances, FlexibleContexts #-}
import qualified Prelude as P
import PredefImports.Predefined
import PredefImports.OpsInHaskell

data Graph =
  Graph' { nodes :: ListOf's Node, edges :: ListOf's Edge }

instance FromTuple2 (ListOf's Node) (ListOf's Edge) Graph where
  ft2 = \(x1, x2) -> Graph' x1 x2

c0nodes :: ListOf's Node -> Graph -> Graph
c0edges :: ListOf's Edge -> Graph -> Graph
c0nodes = \new x -> x { nodes = new }
c0edges = \new x -> x { edges = new }

type Node = P.Integer

data Edge =
  Edge' { u :: Node, v :: Node }

instance FromTuple2 Node Node Edge where
  ft2 = \(x1, x2) -> Edge' x1 x2

c0u :: Node -> Edge -> Edge
c0v :: Node -> Edge -> Edge
c0u = \new x -> x { u = new }
c0v = \new x -> x { v = new }

data IsReprOrHasPar =
  Cis_repr Rank |
  Chas_parent Node

type Rank = P.Integer

data Representative =
  Representative' { name :: Node, rank :: Rank }

instance FromTuple2 Node Rank Representative where
  ft2 = \(x1, x2) -> Representative' x1 x2

c0name :: Node -> Representative -> Representative
c0rank :: Rank -> Representative -> Representative
c0name = \new x -> x { name = new }
c0rank = \new x -> x { rank = new }

type State = (ListOf's Info, ConnComps)

type Info = (Node, IsReprOrHasPar)

type ConnComps = P.Integer

type StateMan = State'Man State

connected_components_of' :: Graph -> ConnComps
connected_components_of' =
  \g ->
  let
  is :: State
  is =
    ft2(apply'to_all_in'((\pA0 -> ft2(pA0, Cis_repr((1 :: P.Integer)))), nodes(g)), a'length(nodes(g)))

  edge_union' :: Edge -> StateMan
  edge_union' =
    \e ->
    find'(u(e)) !>>= \u_rep ->
    find'(v(e)) !>>= \v_rep ->
    (name(u_rep) !== name(v_rep)) &> \pA0 ->
    case pA0 of
      P.True -> do_nothing
      P.False ->
        let
        decrease_cps :: StateMan
        decrease_cps =
          modify_state_with'(\(l, cps) -> ft2(l, cps !- (1 :: P.Integer)))

        new_rank :: Rank
        new_rank =
          rank(u_rep) !+ rank(v_rep)
        in
        decrease_cps !>> 
        (rank(u_rep) !< rank(v_rep)) &> \pA0 ->
        case pA0 of
          P.True -> make'parent_of'with'(name(v_rep), name(u_rep), new_rank)
          P.False -> make'parent_of'with'(name(u_rep), name(v_rep), new_rank)

  make'parent_of'with' :: (Node, Node, Rank) -> StateMan
  make'parent_of'with' =
    \(pn, cn, new_rank) ->
    let
    change_node_list' :: ListOf's Info -> ListOf's Info
    change_node_list' =
      (\pA0 -> apply'to_all_in'((\pA0 -> change_node'if_needed(pA0)), pA0))

    change_node'if_needed :: Info -> Info
    change_node'if_needed =
      \(n, irohp) ->
      (n !== pn) &> \pA0 ->
      case pA0 of
        P.True -> ft2(n, Cis_repr(new_rank))
        P.False ->
          (n !== cn) &> \pA0 ->
          case pA0 of
            P.True -> ft2(n, Chas_parent(pn))
            P.False -> ft2(n, irohp)
    in
    modify_state_with'(\(l, cps) -> ft2(change_node_list'(l), cps))

  find' :: Node -> A'FState'Man Representative State
  find' =
    \n ->
    let
    get_from_info_list :: A'FState'Man IsReprOrHasPar State
    get_from_info_list =
      let
      get_from_list' :: ListOf's Info -> IsReprOrHasPar
      get_from_list' =
        \pA0 ->
        case pA0 of
          [] -> throw_err'("I'm not in the list!")
          (n1, irohp1) : rest ->
            (n !== n1) &> \pA0 ->
            case pA0 of
              P.True -> irohp1
              P.False -> get_from_list'(rest)
      in
      apply'inside'((\x' -> p1st(x')) .> (\pA0 -> get_from_list'(pA0)), get_state)
    in
    get_from_info_list !>>= \pA0 ->
    case pA0 of
      Cis_repr rank -> (\pA0 -> wrap'(pA0)) <& ft2(n, rank)
      Chas_parent p -> find'(p)
  in
  for_all_in''(edges(g), (\pA0 -> edge_union'(pA0))) &> (\pA0 -> final_state_of'on_init_state'(pA0, is)) &> 
  (\x' -> p2nd(x'))

test_graph :: Graph
test_graph =
  ft2
  ( [(1 :: P.Integer), (2 :: P.Integer), (3 :: P.Integer), (4 :: P.Integer), (5 :: P.Integer), (6 :: P.Integer), (7 :: P.Integer), (8 :: P.Integer), (9 :: P.Integer), (10 :: P.Integer)]
  , [ft2((1 :: P.Integer), (2 :: P.Integer)), ft2((2 :: P.Integer), (3 :: P.Integer)), ft2((2 :: P.Integer), (4 :: P.Integer)), ft2((3 :: P.Integer), (4 :: P.Integer)), ft2((5 :: P.Integer), (6 :: P.Integer)), ft2((5 :: P.Integer), (7 :: P.Integer)), ft2((9 :: P.Integer), (10 :: P.Integer))]
  )

main :: IO
main =
  connected_components_of'(test_graph) &> P.print